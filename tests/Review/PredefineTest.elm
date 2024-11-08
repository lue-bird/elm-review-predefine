module Review.PredefineTest exposing (all)

import Review.Predefine
import Review.Test
import Test exposing (Test)


all : Test
all =
    Test.describe "Review.Predefine"
        [ Test.test "should not report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectNoErrors
        , Test.test "should report an error when REPLACEME" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run Review.Predefine.rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "REPLACEME"
                            }
                        ]
        ]
