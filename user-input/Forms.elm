import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Char
import String

main =
  Html.beginnerProgram { model = model, view = view, update = update }

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age : Int
  , hidden: Bool
  }


model : Model
model =
  Model "" "" "" 0 True

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Show


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

    Age age ->
      { model | age = Result.withDefault 0 (String.toInt age) }

    Show ->
      { model | hidden = False }


view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Name", onInput Name ] []
    , input [ type' "password", placeholder "Password", onInput Password ] []
    , input [ type' "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type' "number", placeholder "Age", onInput Age ] []
    , input [ type' "submit", value "Submit", onClick Show ] []
    , viewValidation validatePassword model
    , viewValidation validateAge model
    ]

longEnough pw =
  String.length pw > 8

strongEnough pw =
  String.any Char.isDigit pw
  && String.any Char.isUpper pw
  && String.any Char.isLower pw

validatePassword : Model -> (String, String)
validatePassword model =
  if model.password == model.passwordAgain then
    if longEnough model.password then
      if strongEnough model.password then
        ("green", "Password is OK")
      else ("orange", "Password is not strong enough")
    else ("orange", "Password is too short")
  else
    ("red", "Passwords do not match!")

validateAge : Model -> (String, String)
validateAge model =
  if model.age > 0 then
    ("green", "Age is OK")
  else ("orange", "Invalid age")

viewValidation : (Model -> (String, String)) -> Model -> Html msg
viewValidation validate model =
  let
    (color, message) = validate model
  in
    div [ hidden model.hidden, style [("color", color)] ] [ text message ]
