port module FooBar exposing(..)

import Html exposing (..)
import Html.App exposing App

main = App.program {
  init = init,
  view = view,
  update = update,
  subscriptions = subscriptions
}

type alias Model = { word : String }

init =
  (Model "", Cmd.none)

type Msg = Echo String

port echo : String -> Cmd msg

update msg =
  msg of ->
    Echo word ->
      (Model word, echo word)

