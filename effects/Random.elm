import Html exposing (button, div, Html)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Random

type alias Model =
  { dieFace : Int }

type Msg =
  Roll
  | NewFace Int

main =
  App.program {
    init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
  }

view : Model -> Html Msg
view model =
  div []
    [
    button [ onClick Roll ] [ Html.text "Roll" ]
    , toFace model
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      (model, Random.generate NewFace (Random.int 1 6))

    NewFace newFace ->
      (Model newFace, Cmd.none)

init : (Model, Cmd Msg)
init =
  (Model 1, Cmd.none)

toFace : Model -> Html msg
toFace model =
  case model.dieFace of
    1 -> one
    2 -> two
    _ -> other

one =
  svg
    [ width "100", height "100" ]
    [ polyline [ points "20,20 40,20 40,80 20,80 60,80", fill "none", stroke "black", strokeWidth "2" ] [] ]

two =
  svg
    [ width "100", height "100" ]
    [ polyline [ points "20,20 80,20 80,40 20,40 20,80 80,80", fill "none", stroke "black", strokeWidth "2"] [] ]

other =
  svg
    [ width "100", height "100" ]
    [ polyline [ points "20,80 20,20 80,80 80,20", fill "none", stroke "black", strokeWidth "2" ] [] ]