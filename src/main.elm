import Browser
import Html exposing (Html, text, pre, div)
import Http
import Json.Decode exposing (Decoder, field, string, list, map, map2)
import List exposing(foldr, map)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure Http.Error
  | Loading
  | Success Sections

type alias Section =
  { reference : String
  , markup : String
  }

type alias Sections = List Section


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "/styleguide_test.json"
      , expect = Http.expectJson GotText decodeStyleguideJson
      }
  )



-- UPDATE


type Msg
  = GotText (Result Http.Error Sections)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err error  ->
          (Failure error, Cmd.none)


-- DECODER

decodeStyleguideJson : Decoder Sections
decodeStyleguideJson =
  field "styleguide" (
    field "sections" (
      list (
        map2 Section
          (field "reference" string)
          (field "markup" string)
      )
    )
  )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

httpError: Http.Error -> String
httpError error =
  case error of
          Http.BadBody reason -> reason
          Http.BadStatus code -> (String.fromInt code)
          Http.BadUrl reason -> reason
          Http.NetworkError -> "NetworkError"
          Http.Timeout -> "Timeout"

viewSection: Section -> Html Msg
viewSection section =
  text section.reference

viewSections: Sections -> Html Msg
viewSections sections =
  div [] (map viewSection sections)

view : Model -> Html Msg
view model =
  case model of
    Failure error ->
      text (httpError error)

    Loading ->
      text "Loading..."

    Success sections ->
      viewSections sections
