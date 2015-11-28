module Model where

import BingoUtils as Utils

type alias Entry =
  { phrase : String
  , points: Int
  , wasSpoken: Bool
  , id: Int
  }


type alias Model =
  { entries : List Entry
  , newPhrase : String
  , newPoints : String
  , nextId : Int
  }


type Action
  = NoOp
  | Sort
  | Remove Int
  | Mark Int
  | SetPhrase String
  | SetPoints String
  | Add


update : Action -> Model -> Model
update action model
  = case action of

      NoOp ->
        model

      Sort ->
        { model |
          entries = List.sortBy .points model.entries }

      Remove id ->
        { model |
          entries = List.filter
                (\e -> e.id /= id)
                model.entries }

      Mark id ->
        { model |
          entries = List.map
            (\e ->
              if (e.id == id)
                then { e | wasSpoken = not (e.wasSpoken) }
                else e)
            model.entries }

      SetPhrase p ->
        { model | newPhrase = p }

      SetPoints p ->
        { model | newPoints = p }

      Add ->
        let
          points = Utils.parseInt model.newPoints
          entry = newEntry model.newPhrase points model.nextId
        in
          { model
            | nextId = model.nextId + 1
            , entries = model.entries ++ [ entry ]
            , newPoints = ""
            , newPhrase = ""
          }


calculateScore : List Entry -> Int
calculateScore es =
  let
    marked = List.filter .wasSpoken es
    points = List.map .points marked
  in
    List.sum points


initialModel : Model
initialModel =
  { entries =
      [ newEntry "Future-Proofing" 100 1
      , newEntry "Doing Agile" 300 2
      , newEntry "Synergising" 50 3
      ]
  , newPhrase = ""
  , newPoints = ""
  , nextId = 4
  }


newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
  { phrase = phrase
  , points = points
  , wasSpoken = False
  , id = id
  }
