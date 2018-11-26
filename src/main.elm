import Browser
import Html exposing (Html, text, pre, div)
import Html.Attributes
import Http
import Json.Decode exposing (Decoder, field, string, list, map, map2, null, oneOf)
import List exposing(foldr, map, concatMap)
import Html.Parser



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
      { url = "/styleguide.json"
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

stringOrNull: Decoder String
stringOrNull = oneOf [ string, null "" ]

decodeStyleguideJson : Decoder Sections
decodeStyleguideJson =
  field "sections" (
    list (
      map2 Section
        (field "reference" string)
        (field "markup" stringOrNull)
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


parseMarkup: String -> List Html.Parser.Node
parseMarkup markup =
  case Html.Parser.run markup of
      Result.Ok nodes -> nodes
      Result.Err err -> []

createHtml: Html.Parser.Node -> Html msg
createHtml node =
  case node of
    Html.Parser.Element name attributes children ->
      let convertAttributes = (\ a -> Html.Attributes.attribute (Tuple.first a) (Tuple.second a) ) in
        Html.node name (map convertAttributes attributes) (map createHtml children)
    Html.Parser.Text content ->
      text content
    _ ->
      text ""

viewSection: Section -> List (Html Msg)
viewSection section =
  let m = parseMarkup section.markup in
    List.map createHtml m


view : Model -> Html Msg
view model =
  case model of
    Failure error ->
      text (httpError error)

    Loading ->
      text "Loading..."

    Success sections ->
      div [] (concatMap viewSection sections)
