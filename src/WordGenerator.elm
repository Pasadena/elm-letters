module WordGenerator exposing (lettersToWords, mergeLetterToWord)

import Array

lettersToWords: List Char -> List String
lettersToWords letters =
  case letters of
    [] -> []
    (x::[]) -> [String.fromChar x]
    (x::xs) ->
      let
        letterArray = Array.fromList letters
        arrayLength = Array.length letterArray
        lastLetter = Array.get (arrayLength - 1) letterArray
        remainingLetters = List.take ((List.length letters) - 1) letters
      in
        case lastLetter of
          Nothing -> []
          Just letter -> combinePartialWordsWithLetter (lettersToWords remainingLetters) letter


combinePartialWordsWithLetter: List String -> Char -> List String
combinePartialWordsWithLetter partialWords letter =
  case partialWords of
    [] -> []
    x::xs -> List.append (mergeLetterToWord x letter 0) (combinePartialWordsWithLetter xs letter)

mergeLetterToWord: String -> Char -> Int -> List String
mergeLetterToWord partialWord letter index =
  case index - (String.length partialWord) of
    1 -> []
    _ ->
      let
        letters = String.toList partialWord
        lettersBeforeIndex = List.take index letters
        lettersAfterIndex = List.drop index letters
        beforeAndLetter = List.append lettersBeforeIndex [letter]
        newPartial = List.append beforeAndLetter lettersAfterIndex
      in
        (String.fromList newPartial) :: (mergeLetterToWord partialWord letter (index + 1))
