module Bingo where

import Model exposing (..)
import BingoUtils as Utils

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import List exposing (map)
import String exposing (repeat)
import StartApp.Simple as StartApp
import Signal exposing (Address)


pageHeader : Html
pageHeader = header
  []
  [ h1
      []
      [ text (repeat 3 "Bingo ") ]
  ]


pageFooter : Html
pageFooter = footer
  []
  [ a [ href "http://www.ibm.com" ]
      [ text "IBM" ]
  ]


entryListItem : Address Action -> Entry -> Html
entryListItem address entry =
  li
    [ id (toString entry.id)
    , onClick address (Mark entry.id)
    , classList [ ("highlight", entry.wasSpoken) ]
    ]
    [ span [ class "phrase" ] [ text entry.phrase ]
    , span [ class "points" ] [ text (toString entry.points) ]
    , button
      [ class "delete"
      , onClick address (Remove entry.id)
      ]
      [ ]
    ]


scoreItem : List Entry -> Html
scoreItem entries =
  let
    score = calculateScore entries
  in
    li
      [ class "total" ]
      [ span [ class "label" ] [ text "Score" ]
      , span [ class "points" ] [ text (toString score) ]
      ]


entryList : Address Action -> List Entry -> Html
entryList address entries =
  let
    entryItems = map (entryListItem address) entries
  in
    ul
      []
      ( entryItems ++ [ (scoreItem entries) ] )


sortButton : Address Action -> Html
sortButton address =
  button
    [ class "sort"
    , onClick address Sort
    ]
    [ text "Sort" ]


phraseInput : Address Action -> String -> Html
phraseInput address newPhrase =
  input
      [ type' "text"
      , placeholder "Phrase"
      , value newPhrase
      , Utils.onInput address SetPhrase
      ]
      []


pointsInput : Address Action -> String -> Html
pointsInput address newPoints =
  input
      [ type' "number"
      , value newPoints
      , placeholder "Points"
      , Utils.onInput address SetPoints

      ]
      []


newPhraseEditor : Address Action -> Model -> Html
newPhraseEditor address model =
  div
    []
    [ phraseInput address model.newPhrase
    , pointsInput address model.newPoints
    , button
        [ class "add"
        , onClick address Add

        ]
        [ text "Add"]
    ]


newPhrasePreview : Model -> Html
newPhrasePreview model =
  h2
    []
    [ text (model.newPhrase ++ " " ++ model.newPoints) ]


view : Address Action -> Model -> Html
view address model =
  let
    mainBody =
      case model.entries of
        [] ->
          [ h2 [] [ text "Add some phrases" ] ]
        entries ->
          [ entryList address model.entries
          , sortButton address
          ]
  in
    div [ id "container" ]
      ([ pageHeader
      , newPhraseEditor address model
      , newPhrasePreview model
      ]
      ++ mainBody ++
      [ hr [] []
      , pageFooter
      ])


main : Signal Html
main =
  StartApp.start
    { model = initialModel
    , view = view
    , update = update
    }
