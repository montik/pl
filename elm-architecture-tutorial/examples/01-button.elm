import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = (Int, String)

init : Model
init =
  (0, "The number is not perfect")


-- UPDATE

type Msg = Increment | Decrement | Reset

update : Msg -> Model -> Model
update msg model =
  let (n, _) = model in
  case msg of
    Increment ->
      (n + 1, if n == 4 then "the number is perfect" else "the number is imperfect")

    Decrement ->
      (n - 1, if n == 4 then "the number is perfect" else "the number is imperfect")

    Reset ->
      init


-- VIEW

view : Model -> Html Msg
view model =
  let (n, s) = model in
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text ((String.fromInt n) ++ " " ++ s)]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ button [ onClick Reset ] [ text " RESET!! "] ]
    ]
