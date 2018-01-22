module Tests exposing (..)

import Test exposing (..)
import Test.Runner.Html exposing (run)
import Expect
import WordGenerator exposing (mergeLetterToWord, lettersToWords)


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!

main =
    run <|
      describe "Word generator tests"
          [ test "Single letter produces one single letter word" <|
            \_ -> Expect.equal ["a"] (lettersToWords ['a'])
          , test "Letters a and b produce ab and ba" <|
            \_ ->
              Expect.equal ["ba", "ab"] (lettersToWords ['a', 'b'])
          , test "Letters a, b and c produce abc, acb, bac, bac, cab and cba" <|
            \_ -> Expect.equal ["cba","bca","bac","cab","acb","abc"] (lettersToWords ['a', 'b', 'c'])
          , test "When letter c and word ab is combined, the result is [cab, acb, abc]" <|
              \_ -> Expect.equal ["cab","acb","abc"] (mergeLetterToWord "ab" 'c' 0)
          ]
