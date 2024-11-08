module Review.Predefine exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.Docs
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Infix
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.TypeAnnotation
import Elm.Type
import FastDict
import FastSet
import Review.ModuleNameLookupTable
import Review.Project.Dependency
import Review.Rule


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
rule : Review.Rule.Rule
rule =
    Review.Rule.newProjectRuleSchema "Review.Predefine" initialContext
        |> Review.Rule.withDirectDependenciesProjectVisitor
            (\directDependencies context ->
                ( []
                , { context
                    | functionDeclarationArgumentCounts =
                        FastDict.union
                            (directDependencies
                                |> Dict.foldl
                                    (\_ directDependency soFar ->
                                        FastDict.union
                                            (directDependency |> interfaceDependencyFunctionArgumentCounts)
                                            soFar
                                    )
                                    FastDict.empty
                            )
                            context.functionDeclarationArgumentCounts
                  }
                )
            )
        |> Review.Rule.withContextFromImportedModules
        |> Review.Rule.withModuleVisitor
            (\moduleVisitor ->
                moduleVisitor
                    |> Review.Rule.withDeclarationListVisitor
                        (\declarationNodes context ->
                            ( []
                            , { context
                                | moduleFunctionDeclarationArgumentCounts =
                                    FastDict.union
                                        (declarationNodes
                                            |> List.map Elm.Syntax.Node.value
                                            |> declarationsFunctionArgumentCounts
                                        )
                                        context.moduleFunctionDeclarationArgumentCounts
                              }
                            )
                        )
                    |> Review.Rule.withDeclarationEnterVisitor
                        (\(Elm.Syntax.Node.Node _ declaration) context ->
                            ( []
                            , case declaration of
                                Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                                    let
                                        expressionRange : Elm.Syntax.Range.Range
                                        expressionRange =
                                            valueOrFunctionDeclaration.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .expression
                                                |> Elm.Syntax.Node.range
                                    in
                                    { context
                                        | argumentsInContext =
                                            valueOrFunctionDeclaration.declaration
                                                |> Elm.Syntax.Node.value
                                                |> .arguments
                                                |> List.concatMap patternBindings
                                                |> List.map (\name -> { name = name, scope = expressionRange })
                                    }

                                _ ->
                                    context
                            )
                        )
                    |> Review.Rule.withDeclarationExitVisitor
                        (\_ context ->
                            ( []
                            , { context
                                | argumentsInContext = []
                                , rangesToIgnore = []
                              }
                            )
                        )
                    |> Review.Rule.withExpressionEnterVisitor expressionEnterVisitor
                    |> Review.Rule.withExpressionExitVisitor
                        (\(Elm.Syntax.Node.Node expressionRange _) context ->
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
            )
        |> Review.Rule.withModuleContextUsingContextCreator
            { foldProjectContexts = projectContextsMerge
            , fromProjectToModule = projectToModuleContext
            , fromModuleToProject = moduleToProjectContext
            }
        |> Review.Rule.fromProjectRuleSchema


type alias ProjectContext =
    { functionDeclarationArgumentCounts :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (FastDict.Dict String Int)
    }


type alias ModuleContext =
    { argumentsInContext : List { scope : Elm.Syntax.Range.Range, name : String }
    , rangesToIgnore : List Elm.Syntax.Range.Range
    , moduleFunctionDeclarationArgumentCounts : FastDict.Dict String Int
    , importedFunctionDeclarationArgumentCounts :
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (FastDict.Dict String Int)
    , moduleOriginLookup : Review.ModuleNameLookupTable.ModuleNameLookupTable
    }


initialContext : ProjectContext
initialContext =
    { functionDeclarationArgumentCounts =
        FastDict.empty
    }


projectContextsMerge : ProjectContext -> ProjectContext -> ProjectContext
projectContextsMerge a b =
    { functionDeclarationArgumentCounts =
        FastDict.union
            a.functionDeclarationArgumentCounts
            b.functionDeclarationArgumentCounts
    }


projectToModuleContext : Review.Rule.ContextCreator ProjectContext ModuleContext
projectToModuleContext =
    Review.Rule.initContextCreator
        (\moduleOriginLookup importsProjectContext ->
            { argumentsInContext = []
            , rangesToIgnore = []
            , moduleFunctionDeclarationArgumentCounts = FastDict.empty
            , importedFunctionDeclarationArgumentCounts = importsProjectContext.functionDeclarationArgumentCounts
            , moduleOriginLookup = moduleOriginLookup
            }
        )
        |> Review.Rule.withModuleNameLookupTable


moduleToProjectContext : Review.Rule.ContextCreator ModuleContext ProjectContext
moduleToProjectContext =
    Review.Rule.initContextCreator
        (\moduleName moduleContext ->
            { functionDeclarationArgumentCounts =
                FastDict.singleton moduleName
                    moduleContext.moduleFunctionDeclarationArgumentCounts
            }
        )
        |> Review.Rule.withModuleName


interfaceDependencyFunctionArgumentCounts :
    Review.Project.Dependency.Dependency
    ->
        FastDict.Dict
            Elm.Syntax.ModuleName.ModuleName
            (FastDict.Dict String Int)
interfaceDependencyFunctionArgumentCounts dependency =
    dependency
        |> Review.Project.Dependency.modules
        |> List.foldl
            (\interfaceModule soFar ->
                soFar
                    |> FastDict.insert
                        (interfaceModule.name |> String.split ".")
                        (interfaceModule |> interfaceModuleFunctionArgumentCounts)
            )
            FastDict.empty


interfaceModuleFunctionArgumentCounts : Elm.Docs.Module -> FastDict.Dict String Int
interfaceModuleFunctionArgumentCounts interfaceModule =
    interfaceModule.values
        |> List.foldl
            (\interfaceDeclarationValueOrFunction soFar ->
                case interfaceDeclarationValueOrFunction.tipe |> interfaceTypeFunctionArgumentCount of
                    0 ->
                        soFar

                    argumentCountAtLeast1 ->
                        soFar
                            |> FastDict.insert
                                interfaceDeclarationValueOrFunction.name
                                argumentCountAtLeast1
            )
            FastDict.empty


interfaceTypeFunctionArgumentCount : Elm.Type.Type -> Int
interfaceTypeFunctionArgumentCount type_ =
    case type_ of
        Elm.Type.Lambda _ outType ->
            1 + (outType |> interfaceTypeFunctionArgumentCount)

        _ ->
            0


declarationsFunctionArgumentCounts : List Elm.Syntax.Declaration.Declaration -> FastDict.Dict String Int
declarationsFunctionArgumentCounts declarations =
    declarations
        |> List.foldl
            (\declaration soFar ->
                case declaration of
                    Elm.Syntax.Declaration.FunctionDeclaration valueOrFunctionDeclaration ->
                        let
                            implementation : Elm.Syntax.Expression.FunctionImplementation
                            implementation =
                                valueOrFunctionDeclaration.declaration
                                    |> Elm.Syntax.Node.value

                            argumentCount : Int
                            argumentCount =
                                case valueOrFunctionDeclaration.signature of
                                    Nothing ->
                                        implementation.arguments
                                            |> List.length

                                    Just signature ->
                                        signature
                                            |> Elm.Syntax.Node.value
                                            |> .typeAnnotation
                                            |> Elm.Syntax.Node.value
                                            |> syntaxTypeFunctionArgumentCount
                        in
                        case argumentCount of
                            0 ->
                                soFar

                            argumentCountAtLeast1 ->
                                soFar
                                    |> FastDict.insert (implementation.name |> Elm.Syntax.Node.value)
                                        argumentCountAtLeast1

                    _ ->
                        soFar
            )
            FastDict.empty


syntaxTypeFunctionArgumentCount : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Int
syntaxTypeFunctionArgumentCount type_ =
    case type_ of
        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation _ (Elm.Syntax.Node.Node _ outType) ->
            1 + (outType |> syntaxTypeFunctionArgumentCount)

        _ ->
            0


expressionEnterVisitor :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> ModuleContext
    -> ( List (Review.Rule.Error {}), ModuleContext )
expressionEnterVisitor expressionNode context =
    if
        context.rangesToIgnore
            |> List.any
                (\rangeToIgnore ->
                    rangeToIgnore |> rangeContains (expressionNode |> Elm.Syntax.Node.range)
                )
    then
        ( [], context )

    else
        case expressionIsPrimitive expressionNode of
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
                    argumentsInScope : FastSet.Set String
                    argumentsInScope =
                        context.argumentsInContext
                            |> List.filterMap
                                (\argument ->
                                    if argument.scope |> rangeContains (expressionNode |> Elm.Syntax.Node.range) then
                                        Just argument.name

                                    else
                                        Nothing
                                )
                            |> FastSet.fromList
                in
                if argumentsInScope |> FastSet.isEmpty then
                    ( [], context )

                else if
                    FastSet.intersect
                        (expressionNode |> Elm.Syntax.Node.value |> expressionUsedVariables)
                        argumentsInScope
                        |> FastSet.isEmpty
                then
                    let
                        isValue : Bool
                        isValue =
                            case expressionNode |> expressionToCall of
                                Nothing ->
                                    True

                                Just call ->
                                    let
                                        maybeFullArgumentCount : Maybe Int
                                        maybeFullArgumentCount =
                                            case call.referenceRange |> Review.ModuleNameLookupTable.moduleNameAt context.moduleOriginLookup of
                                                Nothing ->
                                                    Nothing

                                                Just [] ->
                                                    context.moduleFunctionDeclarationArgumentCounts
                                                        |> FastDict.get call.unqualifiedName

                                                Just (moduleNamePart0 :: moduleNamePart1Up) ->
                                                    context.importedFunctionDeclarationArgumentCounts
                                                        |> FastDict.get (moduleNamePart0 :: moduleNamePart1Up)
                                                        |> Maybe.andThen
                                                            (FastDict.get call.unqualifiedName)
                                    in
                                    case maybeFullArgumentCount of
                                        Nothing ->
                                            True

                                        Just fullArgumentCount ->
                                            call.argumentCount >= fullArgumentCount
                    in
                    if isValue then
                        ( Review.Rule.error
                            { message = "value can be pre-defined"
                            , details =
                                [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit."

                                -- , "expressionUsedVariables " ++ (node |> Elm.Syntax.Node.value |> expressionUsedVariables |> FastSet.toList |> String.join " ")
                                -- , "all argumentsInScope " ++ (context.argumentsInContext |> List.map .name |> String.join " ")
                                -- , "argumentsInScope " ++ (argumentsInScope |> FastSet.toList |> String.join " ")
                                ]
                            }
                            (expressionNode |> Elm.Syntax.Node.range)
                            |> List.singleton
                        , { context
                            | rangesToIgnore =
                                context.rangesToIgnore
                                    |> (::) (expressionNode |> Elm.Syntax.Node.range)
                          }
                        )

                    else
                        ( [], context )

                else
                    ( [], context )


rangeContains : Elm.Syntax.Range.Range -> Elm.Syntax.Range.Range -> Bool
rangeContains inner outer =
    Elm.Syntax.Range.combine [ outer, inner ] == outer


expressionUsedVariables : Elm.Syntax.Expression.Expression -> FastSet.Set String
expressionUsedVariables expression =
    FastSet.union
        (case expression of
            Elm.Syntax.Expression.FunctionOrValue [] variable ->
                variable |> FastSet.singleton

            Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ updated) _ ->
                updated |> FastSet.singleton

            _ ->
                FastSet.empty
        )
        (case expression |> expressionSubs of
            [] ->
                FastSet.empty

            sub0 :: sub1Up ->
                (sub0 :: sub1Up)
                    |> List.foldl
                        (\(Elm.Syntax.Node.Node _ sub) soFar ->
                            FastSet.union
                                (sub |> expressionUsedVariables)
                                soFar
                        )
                        FastSet.empty
        )


expressionToCall :
    Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    ->
        Maybe
            { referenceRange : Elm.Syntax.Range.Range
            , unqualifiedName : String
            , argumentCount : Int
            }
expressionToCall (Elm.Syntax.Node.Node fullRange expression) =
    case expression of
        Elm.Syntax.Expression.FunctionOrValue _ unqualifiedName ->
            Just
                { referenceRange = fullRange
                , unqualifiedName = unqualifiedName
                , argumentCount = 0
                }

        Elm.Syntax.Expression.Application (applied :: arguments) ->
            case applied |> expressionToCall of
                Nothing ->
                    Nothing

                Just appliedCall ->
                    Just
                        { referenceRange = appliedCall.referenceRange
                        , unqualifiedName = appliedCall.unqualifiedName
                        , argumentCount =
                            appliedCall.argumentCount
                                + (arguments |> List.length)
                        }

        Elm.Syntax.Expression.OperatorApplication "|>" _ _ applied ->
            case applied |> expressionToCall of
                Nothing ->
                    Nothing

                Just appliedCall ->
                    Just
                        { referenceRange = appliedCall.referenceRange
                        , unqualifiedName = appliedCall.unqualifiedName
                        , argumentCount = appliedCall.argumentCount + 1
                        }

        Elm.Syntax.Expression.OperatorApplication "<|" _ applied _ ->
            case applied |> expressionToCall of
                Nothing ->
                    Nothing

                Just appliedCall ->
                    Just
                        { referenceRange = appliedCall.referenceRange
                        , unqualifiedName = appliedCall.unqualifiedName
                        , argumentCount = appliedCall.argumentCount + 1
                        }

        _ ->
            Nothing


type Flow
    = Primitive
    | Composed
    | IntroducingVariables (List { scope : Elm.Syntax.Range.Range, name : String })


expressionIsPrimitive : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Flow
expressionIsPrimitive (Elm.Syntax.Node.Node expressionRange expression) =
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
                introducedForWholeLetIn : List { name : String, scope : Elm.Syntax.Range.Range }
                introducedForWholeLetIn =
                    letIn.declarations
                        |> List.concatMap
                            (\(Elm.Syntax.Node.Node _ letDeclaration) ->
                                case letDeclaration of
                                    Elm.Syntax.Expression.LetFunction letFunction ->
                                        letFunction.declaration
                                            |> Elm.Syntax.Node.value
                                            |> .name
                                            |> Elm.Syntax.Node.value
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
                        (\( pattern, Elm.Syntax.Node.Node caseExpressionRange _ ) ->
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
                            , scope = lambda.expression |> Elm.Syntax.Node.range
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
    Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> List { scope : Elm.Syntax.Range.Range, name : String }
letDeclarationIntroduced (Elm.Syntax.Node.Node range letDeclaration) =
    case letDeclaration of
        Elm.Syntax.Expression.LetDestructuring _ _ ->
            []

        Elm.Syntax.Expression.LetFunction letValueOrFunctionDeclaration ->
            letValueOrFunctionDeclaration.declaration
                |> Elm.Syntax.Node.value
                |> .arguments
                |> List.concatMap patternBindings
                |> List.map (\name -> { name = name, scope = range })


expressionSubs :
    Elm.Syntax.Expression.Expression
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression)
expressionSubs expression =
    case expression of
        Elm.Syntax.Expression.Application expressions ->
            expressions

        Elm.Syntax.Expression.ListExpr elements ->
            elements

        Elm.Syntax.Expression.RecordExpr fields ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) fields

        Elm.Syntax.Expression.RecordUpdateExpression _ setters ->
            List.map (\(Elm.Syntax.Node.Node _ ( _, expr )) -> expr) setters

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
                    case Elm.Syntax.Node.value declaration of
                        Elm.Syntax.Expression.LetFunction function ->
                            (function.declaration
                                |> Elm.Syntax.Node.value
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


patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ pattern) =
    -- IGNORE TCO
    case pattern of
        Elm.Syntax.Pattern.VarPattern name ->
            name |> List.singleton

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

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
