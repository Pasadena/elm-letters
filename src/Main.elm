import Html exposing (Html, button, div, text, label, input, h3)
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

type Msg = Letter_Added | InputChanged String

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

-- view

view: Model -> Html Msg

view model =
  div [ class "content" ]
    [ div [class "inputs"]
      [ input [ type_ "text", value model.input, onInput InputChanged ] []
        , button [ class "button", onClick Letter_Added ] [ text "Add" ]
      ]
      , h3 [] [ text "You've added these letters:" ]
      , div [class "letters"] (List.map renderLetter model.letters)
      , h3 [] [ text "The generated words are: "]
      , div [class "words"] (List.map renderWord (List.sort model.words))
    ]
renderLetter: String -> Html Msg
renderLetter letter =
  div [ class "letter" ]
  [
    text letter
  ]

renderWord: String -> Html Msg
renderWord word =
  div [class "word"] [ text word ]
