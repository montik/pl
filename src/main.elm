module Main exposing (..)

import Browser
import Html exposing (Html, text, pre, div)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, string, list, map, map2, null, oneOf)
import List exposing(any, foldr, map, concatMap, member)
import Html.Parser



-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- TREE

type alias Node a =
  { id: String
  , path: List String
  , children: Children a
  , data: Maybe a
  }

type Children a = Children (List (Node a))

childrenOf: Node a -> List (Node a)
childrenOf father =
  case father.children of
    Children children -> children

appendChild: Node a -> Node a -> Node a
appendChild child father =
      {father | children = Children(child :: (childrenOf father))}

findChild: String -> Children a -> Maybe (Node a)
findChild id (Children children) =
  case children of
    [] ->
      Nothing
    x :: xs ->
      if (x.id == id) then (Just x)
      else findChild id (Children xs)

insertChild1: a -> (List String) -> Node a -> Node a
insertChild1 node path root =
  case path of
    [] ->
      {root | data = Just node}
    id :: ids ->
      let c = findChild id root.children in
        case c of
          Nothing ->
            let fakeChild = Node id path (Children []) Nothing in
            appendChild (insertChild1 node ids fakeChild) root
          Just x ->
            insertChild1 node ids x


-- RENDER NODE

parseMarkup: String -> List Html.Parser.Node
parseMarkup markup =
  case Html.Parser.run markup of
      Result.Ok nodes -> nodes
      Result.Err err -> []

createHtml: Html.Parser.Node -> Html msg
createHtml node =
  case node of
    Html.Parser.Element name attributes childNodes ->
      let convertAttributes = (\ a -> Html.Attributes.attribute (Tuple.first a) (Tuple.second a) ) in
        Html.node name (map convertAttributes attributes) (map createHtml childNodes)
    Html.Parser.Text content ->
      text content
    _ ->
      text ""

markupOf: Node Section -> String
markupOf node =
  case node.data of
    Nothing -> ""
    Just section -> section.markup

renderNode: Node Section -> List (Html msg)
renderNode node =
  let
    children = childrenOf node
    depth = List.length node.path
    markup = markupOf node
  in
    [ div [class (String.join "." node.path)]
      (List.map createHtml (parseMarkup markup)
       ++ (List.concatMap renderNode children))
    ]


insertSection: Section -> Node Section -> Node Section
insertSection section root =
  let
    path = String.split "." section.reference
  in
    insertChild1 section path root

createTree: Sections -> Node Section
createTree sections =
  let
    root = Node "" [] (Children []) Nothing
  in
    List.foldl insertSection root sections



-- MODEL

type Model
  = Failure Http.Error
  | Loading
  | Success Root

type alias Section =
  { reference : String
  , markup : String
  }

type alias Root = Node Section
type alias Sections = List Section


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "/styleguide.json"
      , expect = Http.expectJson GotText decodeStyleguideJson
      }
  )


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


-- UPDATE

type Msg
  = GotText (Result Http.Error Sections)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok sections ->
            (Success (createTree sections), Cmd.none)
        Err error  ->
          (Failure error, Cmd.none)


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

    Success sectionsTree ->
      div [class "container"] (renderNode sectionsTree)
