import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task

type alias Model =
  { topic : String
  , gifUrl : String
  }

main =
  App.program {
    init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
  }

init : (Model, Cmd Msg)
init =
  (Model "cats" "waiting.gif", Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

type Msg =
  MorePlease
  | FetchSucceed String
  | FetchFail Http.Error

decodeGifUrl : Json.Decoder String
decodeGifUrl =
  Json.at ["data", "image_url"] Json.string

getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomGif model.topic)
    FetchSucceed newUrl ->
      (Model model.topic newUrl, Cmd.none)
    FetchFail _ ->
      (model, Cmd.none)

view : Model -> Html Msg
view model =
  div []
  [ h2 [] [text model.topic]
  , img [src model.gifUrl] []
  , button [onClick MorePlease] [text "More Please!"]
  ]