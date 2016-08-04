module Cell exposing (Model, Msg(Evolve), init, update, view)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Model =
  { x : Int
  , y : Int
  , z : Int
  , alive : Bool
  }

init x y z alive =
  Model x y z alive

type Msg = Evolve (List Model)

reproduce model = { model | alive = True }

die model = { model | alive = False }

-- just for readability
survive model = model

update : Msg -> Model -> Model
update msg model =
  case msg of
    Evolve neighbours ->
      case neighbours of
        [] -> die model -- undercrowded
        h::[] -> die model -- undercrowded
        n ->
          case (List.length n) of
            2 -> survive model
            3 -> reproduce model
            _ -> die model -- overcrowded

view : Model -> Svg a
view model =
  let
    color = if (model.alive) then "green" else "red"
  in
    rect
    [ x (toString model.x)
      , y (toString model.y)
      , width (toString model.z)
      , height (toString model.z)
      , fill color
      , stroke "black"
      , strokeWidth "2px"
    ] []