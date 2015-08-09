module Simulator.Simulation (Model, init, update, view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Util exposing (isEven)
import Util.List as ListUtil exposing (range)

import Simulator.Command exposing (..)
import Simulator.Cell exposing (..)
import Simulator.Unit exposing (..)

type alias Action = ()

type alias Model =
  { width : Int
  , height : Int
  , filled : List Cell
  , unit : Unit
  , units : List Unit
  , commands : List Command
  , gameOver : Bool
  }

init attributes =
  let
    centeredUnits =
      List.map (centerUnit attributes.width) attributes.units

    (unit :: units) = centeredUnits

    fixedAttributes =
      { attributes | units <- units }

    almostModel =
      { fixedAttributes | gameOver = False }

    model : Model
    model =
      { almostModel | unit = unit }
  in
    model

allCells model =
  let
    pairs =
      ListUtil.product (range 0 (model.width - 1)) (range 0 (model.height - 1))
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

  in
    withinBounds model.unit && not (colliding model.unit)

spawnNextUnit : Model -> Model
spawnNextUnit model =
  case model.units of
    (head :: tail) ->
      let
        newModel = { model | unit <- head, units <- tail }
      in
        if isValidPosition newModel then
          newModel
        else
          { newModel | gameOver <- True }

    [] ->
      { model | gameOver <- True }

lockUnitCells : Model -> Model
lockUnitCells model =
  { model | filled <- model.unit.cells ++ model.filled }

clearFullRow : Model -> Int -> Model
clearFullRow model row =
  let
    skipCellsFromRow row cells =
      List.filter (\(x, y) -> y /= row) cells

    shiftDownCell (x, y) =
      moveCell (if isEven y then SE else SW) (x, y)

    shiftDownCellsAboveRow row cells =
      List.map (\(x, y) -> if y < row then shiftDownCell (x, y) else (x, y)) cells

    newFilledCells =
      ((shiftDownCellsAboveRow row) << (skipCellsFromRow row)) model.filled
  in
    { model | filled <- newFilledCells }

clearFullRows : Model -> Model
clearFullRows model =
  let
    rows =
      range 0 (model.height - 1)

    isFullRow row =
      model.width == List.length (List.filter (\(x, y) -> y == row) model.filled)

    firstFullRow =
      ListUtil.find isFullRow rows
  in
    case firstFullRow of
      Just row ->
        clearFullRows (clearFullRow model row)

      Nothing ->
        model

applyCommand : Command -> Model -> Model
applyCommand command model =
  if model.gameOver then
    model
  else
    let
      newModel =
        { model | unit <- commandUnit command model.unit }
    in
      if isValidPosition newModel then
        newModel
      else
        (spawnNextUnit << clearFullRows << lockUnitCells) model

update : Model -> Model
update model =
  case model.commands of
    (command :: tail) ->
      applyCommand command { model | commands <- tail }

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
      unitView model.unit
  in
    div [class "board"]
      (allCellsViews ++ filledCellsViews ++ unitCellsViews)
