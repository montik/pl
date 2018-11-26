import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , age: String
  , submitted: Bool
  }


init : Model
init =
  Model "" "" "" "" False


-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Age String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name,  submitted = False }

    Password password ->
      { model | password = password, submitted = False }

    PasswordAgain password ->
      { model | passwordAgain = password, submitted = False }

    Age age ->
      {model | age = age, submitted = False}

    Submit ->
      {model | submitted = True}

-- HELPER
checkModel: Model -> (Bool, String)
checkModel model =
  if (String.length model.password < 8) then (False, "the password is too short")
  else if (model.password /= model.passwordAgain) then (False, "password do not match")
  else if (String.any Char.isUpper model.password == False) then (False, "password must contain an uppercase ")
  else if (String.any Char.isLower model.password == False) then (False, "password must contain a lowercase")
  else if (String.toInt model.age == Nothing) then (False, "age must be an integer")
  else (True, "")


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewInput "text" "Age" model.age Age
    , viewValidation model
    , button [onClick Submit] [ text "submit!"]
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.submitted then
    let (isValid, error) = checkModel model in
      if isValid then
        div [ style "color" "green" ] [ text "OK" ]
      else
        div [ style "color" "red" ] [ text error]
  else
    div [][]
