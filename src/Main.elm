import Html exposing (Html, button, div, text, label, input, h1, h3)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, class, type_)
import WordGenerator exposing (lettersToWords)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- model

type alias Model =
  {
    letters : List String,
    words : List String,
    input: String
  }

model: Model
model =
  {
    letters = [],
    words = [],
    input = ""
  }

-- update

type Msg = Letter_Added | Letter_Deleted Int | InputChanged String

update: Msg -> Model -> Model
update msg model =
    case msg of
      Letter_Added ->
        let
          withInput = List.append model.letters [model.input]
        in
          { model |
            letters = withInput
            , words = lettersToWords withInput
            , input = ""
          }

      InputChanged value ->
        { model | input = value }

      Letter_Deleted index ->
        let
          withoutLetter = List.append (List.take index model.letters) (List.drop (index +1) model.letters)
        in
          { model |
            letters = withoutLetter
            , words = lettersToWords withoutLetter
          }

-- view

view: Model -> Html Msg

view model =
  div [ class "content" ]
    [ h1 [] [ text "Letters to words" ]
    , div [class "inputs"]
      [ input [ type_ "text", value model.input, onInput InputChanged ] []
        , button [ class "button", onClick Letter_Added ] [ text "Add" ]
      ]
      , renderLetterArea model.letters
      , renderWords model.words
    ]

renderLetterArea: List String -> Html Msg
renderLetterArea letters =
  case letters of
    [] -> div [] []
    _ -> div [] [
      h3 [] [ text "You've added these letters:" ]
      , div [class "letters"] (List.indexedMap renderLetter letters)
    ]

renderLetter: Int -> String -> Html Msg
renderLetter index letter =
  div [ class "letter", onClick (Letter_Deleted index)]
  [
    text letter
  ]

renderWords: List String -> Html Msg
renderWords words =
  case words of
    [] -> div [] []
    _ ->
      let
        sortedWords = List.sort words
        amountOfItems = List.length sortedWords
        isOddAmountOfWords = rem amountOfItems 2 == 0
        middleIndex = if isOddAmountOfWords then (amountOfItems // 2) else (amountOfItems // 2) +1
        firstSetOfWords = List.take middleIndex sortedWords
        secondSetOfWords = List.drop (middleIndex) sortedWords
      in
        div [] [
          h3 [] [ text "The generated words are: "]
          , div [class "words"] [
            div [class "words-column"] (List.map renderWord firstSetOfWords)
            , div [class "words-column"] (List.map renderWord secondSetOfWords)
          ]
        ]

renderWord: String -> Html Msg
renderWord word =
  div [class "word"] [ text word ]
