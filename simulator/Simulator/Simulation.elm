module Simulator.Simulation (Model, update, view, updateOnce) where

import Html exposing (..)
import Html.Attributes exposing (..)

import Simulator.Command exposing (..)
import Simulator.Cell exposing (..)
import Simulator.Unit exposing (..)

type alias Action = ()

type alias Model =
  { width : Int
  , height : Int
  , filled : List Cell
  , unit : Maybe Unit
  , units : List Unit
  , commands : List Command
  , gameOver : Bool
  }

product : List a -> List b -> List (a, b)
product xs ys =
  let product' xs ys =
    case xs of
      x::xs' -> (List.map (\y -> (x, y)) ys)::(product' xs' ys)
      []     -> []
  in List.concat (product' xs ys)

allCells model =
  let
    range a b =
      if a < b then a :: range (a + 1) b else []
    pairs =
      product (range 0 model.width) (range 0 model.height)
  in
    List.map (fromInputCell << InputCell) pairs


-- update

isValidPosition : Model -> Bool
isValidPosition model =
  let
    colliding unit =
      List.any (\cell -> List.member cell unit.cells) model.filled

    cellWithinBounds cell =
      let coords = toInputCellCoords cell
      in
        fst coords >= 0 &&
        fst coords < model.width &&
        snd coords < model.height

    withinBounds unit =
      List.all cellWithinBounds unit.cells

    result unit =
      withinBounds unit && not (colliding unit)
  in
    Maybe.withDefault True (Maybe.map result model.unit)

spawnNextUnit : Model -> Model
spawnNextUnit model =
  case model.units of
    (head :: tail) ->
      { model | unit <- Just (centerUnit model.width head), units <- tail }

    [] ->
      { model | gameOver <- True }

lockUnitCells : Model -> Model
lockUnitCells model =
  let
    unitCells = Maybe.withDefault [] (Maybe.map .cells model.unit)
  in
    { model |
      unit <- Nothing
    , filled <- model.filled ++ unitCells
    }

update : Command -> Model -> Model
update command model =
  if model.gameOver
  then model
  else
  case model.unit of
    Nothing ->
      update command (spawnNextUnit model)

    Just unit ->
      let
        newModel =
          { model | unit <- Just (commandUnit command unit) }
      in
        if isValidPosition newModel then
          newModel
        else
          (spawnNextUnit << lockUnitCells) model

updateOnce : Model -> Model
updateOnce model =
  case model.commands of
    (command :: tail) ->
      update command { model | commands <- tail }

    [] ->
      model

-- view

view address model =
  if model.gameOver then
    div [] [text "Game over!", boardView model]
  else
    boardView model

cellCssPosition : Cell -> List (String, String)
cellCssPosition cell =
  let
    (x, y) = case toInputCell cell of InputCell (x, y) -> (x, y)
    cellSize = 56
    cellSpacing = 2
    w = (cellSize + cellSpacing)
    xPos = x * w + (if y % 2 == 1 then round (0.5 * w) else 0)
    yPos = y * (cellSize - 6)
  in
    [ ("left", (toString xPos) ++ "px")
    , ("top",  (toString yPos) ++ "px")
    ]

cellView : String -> Cell -> Html
cellView color cell =
  div
    [ class ("hexagon hexagon-56 hexagon-" ++ color)
    , style (cellCssPosition cell)
    ]
    [ span [] [ text (toString (x cell) ++ "," ++ toString (y cell))]
    ]

pivotView : Cell -> Html
pivotView pivot =
  div
    [ class ("hexagon hexagon-56 hexagon-dark-gray")
    , style ((cellCssPosition pivot) ++ [("transform", "scale(0.5)")])
    ]
    []

unitView unit =
  (List.map (cellView "violet") unit.cells) ++ [pivotView unit.pivot]

boardView : Model -> Html
boardView model =
  let
    allCellsViews =
      List.map (cellView "gray") (allCells model)

    filledCellsViews =
      List.map (cellView "yellow") (model.filled)

    unitCellsViews =
      Maybe.withDefault [] (Maybe.map unitView model.unit)
  in
    div [class "board"]
      (allCellsViews ++ filledCellsViews ++ unitCellsViews)
