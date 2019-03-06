import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
  { c : String
  , f : String
  }

init : Model
init =
  { c = "0"
  , f = "0"
  }

-- UPDATE

type Msg
  = ConvertC String
  | ConvertF String

cToF s =
  String.fromFloat ((Maybe.withDefault 0 (String.toFloat s)) + 32)

fToC s =
  String.fromFloat ((Maybe.withDefault 0 (String.toFloat s)) - 32)

update : Msg -> Model -> Model
update msg model =
  case msg of
    ConvertC newContent ->
      case (String.toFloat newContent) of
        Nothing ->
          { model | c = newContent }
        _ ->
          { model | c = newContent, f = cToF newContent }
    ConvertF newContent ->
      case (String.toFloat newContent) of
        Nothing ->
          { model | f = newContent }
        _ ->
          { model | c = fToC newContent, f = newContent }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "C", value model.c, onInput ConvertC ] []
    , input [ placeholder "F", value model.f, onInput ConvertF ] []
    ]
