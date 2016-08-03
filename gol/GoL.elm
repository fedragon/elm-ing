import Cell

import Html exposing (Html, div, span, text)
import Html.App as App
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Time exposing (every, second, Time)

type alias Model =
  { generation : Int
  , cell : Cell.Model
  }

main =
  App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }

init : (Model, Cmd Msg)
init =
  (Model 0 (Cell.init 50 50), Cmd.none)

type Msg = NextGen Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NextGen t ->
      ({ model | generation = model.generation + 1 }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second NextGen

view : Model -> Html a
view model =
  div [] [
    span [] [text ("Generation: " ++ (toString model.generation))],
    svg [ viewBox "0 0 300 300", height "300px", width "300px" ]
      [ (Cell.view model.cell) ]
  ]