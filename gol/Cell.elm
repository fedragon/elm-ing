module Cell exposing (Model, Msg, init, update, view)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Model =
  { x : Int
  , y : Int
  , alive : Bool
  }

init x y =
  Model x y True

type Msg = Die

update msg model =
  case msg of
    Die ->
      { model | alive = False }

view : Model -> Svg a
view model =
  let
    color = if (model.alive) then "green" else "red"
  in
    circle [ cx (toString model.x), cy (toString model.y), r "10", fill color ] []
