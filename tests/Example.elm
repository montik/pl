module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Main exposing (..)


suite : Test
suite =
    describe "The String module"
        [ describe "String.reverse" -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]

treeDimension: Node a -> Int
treeDimension root =
  1 + List.sum (List.map treeDimension (childrenOf root))

createTreeTest: Test
createTreeTest =
  let
    sections =
      [ Section "atom" "<h1>atom</h1>"
      , Section "atom.icon" "<h2>atom icon</h2>"
      , Section "atom.form" "<h2>atom icon</h2>"
      , Section "atom.button" "<h2>button</h2>"
      , Section "molecule" "<h2>button</h2>"
      , Section "molecule.teaser" "<h2>button</h2>"
      , Section "molecule.teaser.solid" "<h2>button</h2>"
      ]
    tree = createTree sections
  in
    describe "Correctely create a tree from a list of sections"
    [ test "The root has no id" <|
      \_ -> Expect.equal "" tree.id

    , describe "Insert the right number of child"
      [ test "The root element should have as many children as main sections" <|
        let
          isMainSection = \s -> (List.length (String.split "." s.reference)) == 1
          numberOfMainSections = (List.length (List.filter isMainSection sections))
        in
          \_ ->
            Expect.equal numberOfMainSections (List.length (childrenOf tree))
      , test "The size of the tree equals the number of input sections plus the added root" <|
        \_ ->
          Expect.equal (1 + List.length sections) (treeDimension tree)
      ]
    ]

findNodeTest: Test
findNodeTest =
  let
    dummySection = Section "dummy" "<h2>dummy</h2>"
    atoms =
      Node "atom" []
      (Children
        [ Node "icon" [] (Children []) (Just dummySection)
        , Node "button" [] (Children []) (Just dummySection)
        , Node "form" [] (Children []) (Just dummySection)
      ])
      Nothing
    molecules =
      Node "molecules" []
      (Children
        [ Node "icon" [] (Children []) (Just dummySection)
        , Node "button" [] (Children []) (Just dummySection)
        , Node "button" [] (Children []) (Just dummySection)
      ])
      Nothing
    tree = Node "" [] (Children [atoms, molecules]) Nothing
    atomIcon = findNode ["atom", "icon"] tree
  in
    describe "Correctely find the node"
    [ test "atom.icon" <|
      \_ ->
        case atomIcon of
          Just sec ->
            Expect.equal sec.id "icon"
          Nothing ->
            Expect.fail "No node found"
    , test "atom has the right number of children" <|
      \_ ->
        let
          atom = findNode ["atom"] tree
        in
          case atom of
            Just a ->
              Expect.equal (List.length (childrenOf a)) 3
            Nothing ->
              Expect.fail "No node found"
    ]
