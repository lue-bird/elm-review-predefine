module Review.PredefineTest exposing (all)

import Review.Predefine
import Review.Test
import Test exposing (Test)


all : Test
all =
    Test.describe "Review.Predefine"
        [ Test.test "should not report an error when imported function from dependency is curried"
            (\() ->
                """module A exposing (..)
a b =
    List.map identity
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function without signature is curried"
            (\() ->
                """module A exposing (..)
a b =
    listMap identity

listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature is curried"
            (\() ->
                """module A exposing (..)
a b =
    listMap identity

listMap : (a -> b) -> List a -> List b
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature where out type function is parenthesized is curried"
            (\() ->
                """module A exposing (..)
a b =
    listMap identity

listMap : (a -> b) -> (List a -> List b)
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when imported function from project without signature is curried"
            (\() ->
                [ """module A exposing (..)
import List2
a b =
    List2.map identity
"""
                , """module List2 exposing (map)

map f list =
    List.map f list
"""
                ]
                    |> Review.Test.runOnModules Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature is fully applied but the last argument uses an argument"
            (\() ->
                """module A exposing (..)
a b =
    listMap identity b

listMap : (a -> b) -> List a -> List b
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature is fully applied but the last argument uses an argument from last case-of branch"
            (\() ->
                """module A exposing (..)
a b =
    case b of
        [] ->
            []
        list ->
            listMap identity list

listMap : (a -> b) -> List a -> List b
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature is fully applied but the last argument uses arguments from first case-of branch in application"
            (\() ->
                """module A exposing (..)
a b =
    case b of
        bHead::bTail ->
            listMap identity (identity (bHead::bTail))
        [] ->
            []

listMap : (a -> b) -> List a -> List b
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature is fully applied but the last argument (applied with |>) uses arguments from first case-of branch in application"
            (\() ->
                """module A exposing (..)
a b =
    case b of
        bHead::bTail ->
            identity (bHead::bTail)
                |> listMap identity
        [] ->
            []

listMap : (a -> b) -> List a -> List b
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when module-declared function with signature is fully applied but the last argument (applied with |>) uses argument from let-in in application"
            (\() ->
                """module A exposing (..)
a b =
    let bAgain = b
    in
    identity bAgain
        |> listMap identity

listMap : (a -> b) -> List a -> List b
listMap f list =
    List.map f list
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when imported function from project without signature is fully applied but last argument uses argument"
            (\() ->
                [ """module A exposing (..)
import List2
a b =
    List2.map identity
"""
                , """module List2 exposing (map)

map f list =
    List.map f list
"""
                ]
                    |> Review.Test.runOnModules Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when imported function from project without signature is fully applied but last argument uses argument in application"
            (\() ->
                [ """module A exposing (..)
import List2
a b =
    List2.map identity (identity b)
"""
                , """module List2 exposing (map)

map f list =
    List.map f list
"""
                ]
                    |> Review.Test.runOnModules Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should not report an error when imported function from project without signature is fully applied but last argument (applied with |>) uses argument in application"
            (\() ->
                [ """module A exposing (..)
import List2
a b =
    identity b |> List2.map identity
"""
                , """module List2 exposing (map)

map f list =
    List.map f list
"""
                ]
                    |> Review.Test.runOnModules Review.Predefine.rule
                    |> Review.Test.expectNoErrors
            )
        , Test.test "should report an error when function is fully applied"
            (\() ->
                """module A exposing (..)
a b =
    List.map identity []
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "List.map identity []"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied via |>"
            (\() ->
                """module A exposing (..)
a b =
    [] |> List.map identity
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "[] |> List.map identity"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied via <|"
            (\() ->
                """module A exposing (..)
a b =
    List.map identity <| []
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "List.map identity <| []"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied multiple times via |>"
            (\() ->
                """module A exposing (..)
a b =
    [] |> identity |> identity
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "[] |> identity |> identity"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied multiple times via <|"
            (\() ->
                """module A exposing (..)
a b =
    identity <| identity <| []
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "identity <| identity <| []"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied via <| into parenthesized <|"
            (\() ->
                """module A exposing (..)
a b =
    (List.map <| identity) <| []
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "(List.map <| identity) <| []"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied via <| into parenthesized |>"
            (\() ->
                """module A exposing (..)
a b =
    (identity |> List.map) <| []
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "(identity |> List.map) <| []"
                            }
                        ]
            )
        , Test.test "should report an error when function is fully applied via <| into parenthesized application"
            (\() ->
                """module A exposing (..)
a b =
    (List.map identity) <| []
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "value can be pre-defined"
                            , details = [ "Since this value doesn't rely on any arguments, it can be moved to the module level, which comes with a performance benefit." ]
                            , under = "(List.map identity) <| []"
                            }
                        ]
            )
        ]
