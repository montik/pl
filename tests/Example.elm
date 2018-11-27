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


createTreeTest: Test
createTreeTest =
  let
    sections =
      [ Section "atom" "<h1>atom</h1>"
      , Section "atom.icon" "<h2>atom icon</h2>"
      , Section "button" "<h2>button</h2>"
      ]
  in
    describe "Correctely create a tree from a list of sections"
    [ describe "Insert the right number of child"
      [ test "two expected" <|
        \_ ->
          let
            tree = createTree sections
          in
            Expect.equal (List.length (childrenOf tree)) 2
      , test "three expected" <|
        \_ ->
          let
            moreSections =
              sections ++ [ Section "form" "<h1>form</h1>" ]
            tree = createTree moreSections
          in
            Expect.equal (List.length (childrenOf tree)) 3
      ]
    ]

renderNodeTest: Test
renderNodeTest =
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
      Node "atom" []
      (Children
        [ Node "icon" [] (Children []) (Just dummySection)
        , Node "button" [] (Children []) (Just dummySection)
        , Node "button" [] (Children []) (Just dummySection)
      ])
      Nothing
    tree = Node "" [] (Children [atoms, molecules]) Nothing
    markup = renderNode tree
