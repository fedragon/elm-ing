module Counter exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type alias Model =
  { count : Int
  , max : Int
  , min : Int
  }

init : Int -> Model
init count =
  Model count count count

type Msg
  = Increment
  | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      let
        newCount = model.count + 1
      in
        { model |
          count = newCount
          , max = if (newCount > model.max) then newCount else model.max
        }
    Decrement ->
      let
        newCount = model.count - 1
      in
      { model |
        count = newCount
        , min = if (newCount < model.min) then newCount else model.min
      }

view : Model -> Html Msg
view model =
  div []
    [ div [ countStyle ] [ text (toString model.min) ]
    , button [ onClick Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [ countStyle ] [ text (toString model.max) ]
    ]

countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]