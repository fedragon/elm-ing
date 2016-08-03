import Cell
import Html exposing (Html, button, div, span, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes exposing (height, viewBox, width)
import Time exposing (Time, every, second)

type alias Model =
  { generation : Int
  , cells : List Cell.Model
  , paused : Bool
  }

type Msg = NextGen Time | Pause

main =
  App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }

init : (Model, Cmd Msg)
init =
  let
    boardSize = 4
    cellSize = 30
    board =
      List.concatMap
        (\i ->
          List.map
            (\j ->
              let alive = (i == 1 || j == 4)
              in (Cell.Model (i * cellSize) (j * cellSize) cellSize alive))
            [0..boardSize])
        [0..boardSize]
  in
    (Model 0 board False, Cmd.none)

aliveNeighboursOf : Cell.Model -> List Cell.Model -> List Cell.Model
aliveNeighboursOf cell cells =
  List.filter
    (\other ->
      other.alive == True
      && cell /= other
      && (abs cell.x - other.x) <= cell.z
      && (abs cell.y - other.y) <= cell.z)
    cells

updateCell : Cell.Model -> List Cell.Model -> Cell.Model
updateCell cell cells =
  let
    neighbours = aliveNeighboursOf cell cells
  in
    (Cell.update (Cell.Evolve neighbours) cell)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pause ->
      ({ model | paused = (not model.paused) }, Cmd.none)
    NextGen _ ->
      ({ model |
        generation = model.generation + 1
        , cells = List.map (\cell -> (updateCell cell model.cells)) model.cells
        },
        Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if (model.paused || List.all (\x -> not x.alive) model.cells) then
    Sub.none
  else
    Time.every second NextGen

view : Model -> Html Msg
view model =
  div [] [
    span [] [text ("Generation: " ++ (toString model.generation))],
    button [onClick Pause] [text "Pause/Resume"],
    svg [ viewBox "0 0 300 300", height "300px", width "300px" ]
      (List.map Cell.view model.cells)
  ]
