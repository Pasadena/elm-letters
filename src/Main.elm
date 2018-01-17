import Html exposing (Html, button, div, text, label, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)

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
        { model | letters = List.append model.letters [model.input] }

      InputChanged value ->
        { model | input = value }

-- view

view: Model -> Html Msg

view model =
  div []
    [ input [ onInput InputChanged ] []
      , button [ onClick Letter_Added ] [ text "Add" ]
      , div [] [ text (toString model.letters) ]
    ]