module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, pre, div)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, string, list, map, map2, null, oneOf)
import List exposing(any, foldr, map, concatMap, member)
import Html.Parser
import Url



-- MAIN

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
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

findChild: String -> Children a -> Maybe (Node a)
findChild id (Children children) =
  case children of
    [] ->
      Nothing
    x :: xs ->
      if (x.id == id) then (Just x)
      else findChild id (Children xs)

findNode: List String -> Node a -> Maybe ( Node a )
findNode path root =
  case path of
    [] ->
      Just root
    id :: ids ->
      case (findChild id root.children) of
        Nothing ->
          Nothing
        Just node ->
          findNode ids node

-- TODO test if a faster solution makes sense
insertChild: a -> (List String) -> Node a -> Node a
insertChild node currentPath root =
  case currentPath of
    [] ->
      {root | data = Just node}
    id :: ids ->
      let
        child = findChild id root.children
      in
        case child of
          Nothing ->
            let
              path = root.path ++ [ id ]
              fakeChild = Node id path (Children []) Nothing
              children = childrenOf root
              newChild = insertChild node ids fakeChild
            in
              {root | children = Children ( newChild :: children )}
          Just existingChild  ->
            let
              newChild = insertChild node ids existingChild
              children = List.map (\n -> if (n.id == newChild.id) then newChild else n) (childrenOf root)
            in
              {root | children = Children ( children )}


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
      let
        convertAttributes = (\ a -> Html.Attributes.attribute (Tuple.first a) (Tuple.second a) )
      in
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
    markup = markupOf node
  in
    [ div [class (String.join "." node.path)]
      ( List.map createHtml (parseMarkup markup) ++
        (List.concatMap renderNode children))
    ]


insertSection: Section -> Node Section -> Node Section
insertSection section root =
  let
    path = String.split "." section.reference
  in
    insertChild section path root

createTree: Sections -> Node Section
createTree sections =
  let
    root = Node "" [] (Children []) Nothing
  in
    List.foldl insertSection root sections



-- MODEL

type Model
  = Failure
    { error: Http.Error
    , url: Url
    }

  | Loading
    { url: Url
    }

  | Success
    { sectionsTree: Root
    , url: Url
    }


type alias Url =
    { key : Nav.Key
    , url : Url.Url
    }

type alias Section =
  { reference : String
  , markup : String
  }

type alias Root = Node Section
type alias Sections = List Section


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  ( Loading url key
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
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok sections ->
            (Success (createTree sections) model.url model.key, Cmd.none)
        Err error  ->
          (Failure error model.url model.key, Cmd.none)

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
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

view: Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body =
    [ case model of
        Failure error url ->
          text (httpError error)

        Loading url ->
          text "Loading..." ++ url

        Success sectionsTree url ->
          text url
          --div [class "container"] (renderNode sectionsTree)
    ]
  }
