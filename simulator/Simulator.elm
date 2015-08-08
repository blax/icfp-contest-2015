module Simulator where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)

import Simulator.Move exposing (..)
import Simulator.Cell exposing (..)
import Simulator.Unit exposing (..)

type InputCell = InputCell (Int, Int)

type alias Model =
  { width : Int
  , height : Int
  , filled : List Cell
  , unit : Unit
  }

type alias Action = ()


sgn : Int -> Int
sgn x =
  if x < 0 then -1 else 1

fromInputCell : InputCell -> Cell
fromInputCell (InputCell (x, y)) =
  (x - floor (toFloat y / 2), y)

toInputCell : Cell -> InputCell
toInputCell (x, y) =
  InputCell (x + floor (toFloat y / 2), y)

inputCommands = [Move SE, Rotate CCW]

main =
  let
    applyCommand command model =
      { model | unit <- commandUnit command model.unit }

    model =
      List.foldl applyCommand initialModel inputCommands

    app =
      { model = model, view = view, update = update }
  in
    StartApp.start app

-- model

inputModel =
  { width = 15
  , height = 15
  , filled = []
  , unit = { cells = [InputCell (0, 2)], pivot = InputCell (0, 0) }
  }

fromInputUnit inputUnit =
  { inputUnit |
    cells <- (List.map fromInputCell inputUnit.cells)
  , pivot <- fromInputCell inputUnit.pivot
  }

initialModel : Model
initialModel =
  { inputModel |
    filled <- List.map fromInputCell inputModel.filled
  , unit <- fromInputUnit inputModel.unit
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

update : Action -> Model -> Model
update action model =
  model

-- view

view address model =
  div []
    [ boardView model
    ]

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

boardView : Model -> Html
boardView model =
  let
    allCellsViews =
      List.map (cellView "gray") (allCells model)

    filledCellsViews =
      List.map (cellView "yellow") (model.filled)

    unitCellsViews unit =
      (List.map (cellView "violet") unit.cells) ++ [pivotView unit.pivot]
  in
    div [class "board"]
      (allCellsViews ++ filledCellsViews ++ unitCellsViews model.unit)
