module Review.Predefine exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern
import Elm.Syntax.Range as Range exposing (Range)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports values that are constructed lazily but could be defined at the module level to save allocations.

    config =
        [ Review.Predefine.rule
        ]

Only use this rule if performance is critical and every trick in the hat is important.


## try

```bash
elm-review --template lue-bird/elm-review-predefine/example
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "Review.Predefine" initialContext
        |> Rule.withDeclarationEnterVisitor
            (\(Node _ declaration) context ->
                ( []
                , case declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                        let
                            expressionRange : Range
                            expressionRange =
                                valueOrFunctionDeclaration.declaration
                                    |> Node.value
                                    |> .expression
                                    |> Node.range
                        in
                        { context
                            | argumentsInContext =
                                valueOrFunctionDeclaration.declaration
                                    |> Node.value
                                    |> .arguments
                                    |> List.concatMap patternBindings
                                    |> List.map (\name -> { name = name, scope = expressionRange })
                        }

                    _ ->
                        context
                )
            )
        |> Rule.withDeclarationExitVisitor
            (\_ context ->
                ( []
                , { context
                    | argumentsInContext = []
                    , rangesToIgnore = []
                  }
                )
            )
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor
            (\(Node expressionRange _) context ->
                ( []
                , { context
                    | argumentsInContext =
                        context.argumentsInContext
                            |> List.filterMap
                                (\scopedVariable ->
                                    if expressionRange |> rangeContains scopedVariable.scope then
                                        Nothing

                                    else
                                        Just scopedVariable
                                )
                    , rangesToIgnore =
                        context.rangesToIgnore
                            |> List.filterMap
                                (\rangeToIgnore ->
                                    if expressionRange |> rangeContains rangeToIgnore then
                                        Nothing

                                    else
                                        Just rangeToIgnore
                                )
                  }
                )
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    { argumentsInContext : List { scope : Range, name : String }
    , rangesToIgnore : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            { argumentsInContext = []
            , rangesToIgnore = []
            }
        )


expressionEnterVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionEnterVisitor node context =
    if context.rangesToIgnore |> List.any (\rangeToIgnore -> rangeToIgnore |> rangeContains (node |> Node.range)) then
        ( [], context )

    else
        case expressionIsPrimitive node of
            Primitive ->
                ( [], context )

            IntroducingVariables introducingVariables ->
                ( []
                , { context
                    | argumentsInContext =
                        context.argumentsInContext
                            ++ introducingVariables
                  }
                )

            Composed ->
                let
                    argumentsInScope : Set String
                    argumentsInScope =
                        context.argumentsInContext
                            |> List.filterMap
                                (\argument ->
                                    if argument.scope |> rangeContains (node |> Node.range) then
                                        Just argument.name

                                    else
                                        Nothing
                                )
                            |> Set.fromList
                in
                if argumentsInScope |> Set.isEmpty then
                    ( [], context )

                else if Set.intersect (node |> Node.value |> expressionUsedVariables) argumentsInScope |> Set.isEmpty then
                    ( Rule.error
                        { message = "value can be pre-defined"
                        , details =
                            [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit."
                            , "expressionUsedVariables " ++ (node |> Node.value |> expressionUsedVariables |> Set.toList |> String.join " ")
                            , "all argumentsInScope " ++ (context.argumentsInContext |> List.map .name |> String.join " ")
                            , "argumentsInScope " ++ (argumentsInScope |> Set.toList |> String.join " ")
                            ]
                        }
                        (Node.range node)
                        |> List.singleton
                    , { context
                        | rangesToIgnore =
                            context.rangesToIgnore
                                |> (::) (node |> Node.range)
                      }
                    )

                else
                    ( [], context )


rangeContains : Range -> Range -> Bool
rangeContains inner outer =
    Range.combine [ outer, inner ] == outer


expressionUsedVariables : Expression -> Set String
expressionUsedVariables =
    \expression ->
        Set.union
            (case expression of
                Elm.Syntax.Expression.FunctionOrValue [] variable ->
                    variable |> Set.singleton

                Elm.Syntax.Expression.RecordUpdateExpression (Node _ updated) _ ->
                    updated |> Set.singleton

                _ ->
                    Set.empty
            )
            (case expression |> expressionSubs of
                [] ->
                    Set.empty

                sub0 :: sub1Up ->
                    (sub0 :: sub1Up)
                        |> List.foldl
                            (\(Node _ sub) soFar ->
                                Set.union
                                    (sub |> expressionUsedVariables)
                                    soFar
                            )
                            Set.empty
            )


type Flow
    = Primitive
    | Composed
    | IntroducingVariables (List { scope : Range, name : String })


expressionIsPrimitive : Node Expression -> Flow
expressionIsPrimitive =
    \(Node expressionRange expression) ->
        case expression of
            Elm.Syntax.Expression.UnitExpr ->
                Primitive

            Elm.Syntax.Expression.Application _ ->
                Composed

            Elm.Syntax.Expression.OperatorApplication _ _ _ _ ->
                Composed

            Elm.Syntax.Expression.FunctionOrValue _ _ ->
                Primitive

            Elm.Syntax.Expression.IfBlock _ _ _ ->
                Composed

            Elm.Syntax.Expression.PrefixOperator _ ->
                Primitive

            Elm.Syntax.Expression.Operator _ ->
                Primitive

            Elm.Syntax.Expression.Integer _ ->
                Primitive

            Elm.Syntax.Expression.Hex _ ->
                Primitive

            Elm.Syntax.Expression.Floatable _ ->
                Primitive

            Elm.Syntax.Expression.Negation _ ->
                Composed

            Elm.Syntax.Expression.Literal _ ->
                Primitive

            Elm.Syntax.Expression.CharLiteral _ ->
                Primitive

            Elm.Syntax.Expression.TupledExpression _ ->
                Composed

            Elm.Syntax.Expression.ParenthesizedExpression inParens ->
                inParens |> expressionIsPrimitive

            Elm.Syntax.Expression.LetExpression letIn ->
                let
                    introducedForWholeLetIn : List { name : String, scope : Range }
                    introducedForWholeLetIn =
                        letIn.declarations
                            |> List.concatMap
                                (\(Node _ letDeclaration) ->
                                    case letDeclaration of
                                        Elm.Syntax.Expression.LetFunction letFunction ->
                                            letFunction.declaration
                                                |> Node.value
                                                |> .name
                                                |> Node.value
                                                |> List.singleton

                                        Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                            patternNode |> patternBindings
                                )
                            |> List.map (\name -> { name = name, scope = expressionRange })
                in
                IntroducingVariables
                    (introducedForWholeLetIn
                        ++ (letIn.declarations |> List.concatMap letDeclarationIntroduced)
                    )

            Elm.Syntax.Expression.CaseExpression caseOf ->
                IntroducingVariables
                    (caseOf.cases
                        |> List.concatMap
                            (\( pattern, Node caseExpressionRange _ ) ->
                                pattern
                                    |> patternBindings
                                    |> List.map (\name -> { name = name, scope = caseExpressionRange })
                            )
                    )

            Elm.Syntax.Expression.LambdaExpression lambda ->
                IntroducingVariables
                    (lambda.args
                        |> List.concatMap patternBindings
                        |> List.map
                            (\name ->
                                { name = name
                                , scope = lambda.expression |> Node.range
                                }
                            )
                    )

            Elm.Syntax.Expression.RecordExpr _ ->
                Composed

            Elm.Syntax.Expression.ListExpr elements ->
                case elements of
                    [] ->
                        Primitive

                    _ :: _ ->
                        Composed

            Elm.Syntax.Expression.RecordAccess record _ ->
                record |> expressionIsPrimitive

            Elm.Syntax.Expression.RecordAccessFunction _ ->
                Primitive

            Elm.Syntax.Expression.RecordUpdateExpression _ _ ->
                Composed

            Elm.Syntax.Expression.GLSLExpression _ ->
                Primitive


letDeclarationIntroduced :
    Node Elm.Syntax.Expression.LetDeclaration
    -> List { scope : Range, name : String }
letDeclarationIntroduced =
    \(Node range letDeclaration) ->
        case letDeclaration of
            Elm.Syntax.Expression.LetDestructuring _ _ ->
                []

            Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
                letValueOrFunctionDeclaration.declaration
                    |> Node.value
                    |> .arguments
                    |> List.concatMap patternBindings
                    |> List.map (\name -> { name = name, scope = range })


expressionSubs :
    Elm.Syntax.Expression.Expression
    -> List (Node Elm.Syntax.Expression.Expression)
expressionSubs =
    \expression ->
        case expression of
            Elm.Syntax.Expression.Application expressions ->
                expressions

            Elm.Syntax.Expression.ListExpr elements ->
                elements

            Elm.Syntax.Expression.RecordExpr fields ->
                List.map (\(Node _ ( _, expr )) -> expr) fields

            Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
                List.map (\(Node _ ( _, expr )) -> expr) setters

            Elm.Syntax.Expression.ParenthesizedExpression expr ->
                [ expr ]

            Elm.Syntax.Expression.OperatorApplication _ direction left right ->
                case direction of
                    Elm.Syntax.Infix.Left ->
                        [ left, right ]

                    Elm.Syntax.Infix.Right ->
                        [ right, left ]

                    Elm.Syntax.Infix.Non ->
                        [ left, right ]

            Elm.Syntax.Expression.IfBlock cond then_ else_ ->
                [ cond, then_, else_ ]

            Elm.Syntax.Expression.LetExpression letIn ->
                List.foldr
                    (\declaration soFar ->
                        case Node.value declaration of
                            Elm.Syntax.Expression.LetFunction function ->
                                (function.declaration
                                    |> Node.value
                                    |> .expression
                                )
                                    :: soFar

                            Elm.Syntax.Expression.LetDestructuring _ expr ->
                                expr :: soFar
                    )
                    [ letIn.expression ]
                    letIn.declarations

            Elm.Syntax.Expression.CaseExpression caseOf ->
                caseOf.expression
                    :: List.map (\( _, caseExpression ) -> caseExpression) caseOf.cases

            Elm.Syntax.Expression.LambdaExpression lambda ->
                [ lambda.expression ]

            Elm.Syntax.Expression.TupledExpression expressions ->
                expressions

            Elm.Syntax.Expression.Negation expr ->
                [ expr ]

            Elm.Syntax.Expression.RecordAccess expr _ ->
                [ expr ]

            Elm.Syntax.Expression.PrefixOperator _ ->
                []

            Elm.Syntax.Expression.Operator _ ->
                []

            Elm.Syntax.Expression.Integer _ ->
                []

            Elm.Syntax.Expression.Hex _ ->
                []

            Elm.Syntax.Expression.Floatable _ ->
                []

            Elm.Syntax.Expression.Literal _ ->
                []

            Elm.Syntax.Expression.CharLiteral _ ->
                []

            Elm.Syntax.Expression.UnitExpr ->
                []

            Elm.Syntax.Expression.FunctionOrValue _ _ ->
                []

            Elm.Syntax.Expression.RecordAccessFunction _ ->
                []

            Elm.Syntax.Expression.GLSLExpression _ ->
                []


patternBindings : Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings =
    -- IGNORE TCO
    \(Node _ pattern) ->
        case pattern of
            Elm.Syntax.Pattern.VarPattern name ->
                name |> List.singleton

            Elm.Syntax.Pattern.AsPattern afterAsPattern (Node _ name) ->
                name :: (afterAsPattern |> patternBindings)

            Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
                inParens |> patternBindings

            Elm.Syntax.Pattern.ListPattern patterns ->
                patterns |> List.concatMap patternBindings

            Elm.Syntax.Pattern.TuplePattern patterns ->
                patterns |> List.concatMap patternBindings

            Elm.Syntax.Pattern.RecordPattern fields ->
                fields |> List.map Node.value

            Elm.Syntax.Pattern.NamedPattern _ patterns ->
                patterns |> List.concatMap patternBindings

            Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
                (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

            Elm.Syntax.Pattern.AllPattern ->
                []

            Elm.Syntax.Pattern.UnitPattern ->
                []

            Elm.Syntax.Pattern.CharPattern _ ->
                []

            Elm.Syntax.Pattern.StringPattern _ ->
                []

            Elm.Syntax.Pattern.IntPattern _ ->
                []

            Elm.Syntax.Pattern.HexPattern _ ->
                []

            Elm.Syntax.Pattern.FloatPattern _ ->
                []
