module Simulator where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)

import Simulator.Move exposing (..)
import Simulator.Cell exposing (..)
import Simulator.Unit exposing (..)

type alias Model =
  { width : Int
  , height : Int
  , filled : List Cell
  , unit : Unit
  }

type alias Action = ()

input = [SE, SW, E]

main =
  let
    model =
      List.foldl (\move model -> { model | unit <- moveUnit move model.unit }) initialModel input

    app =
      { model = model, view = view, update = update }
  in
    StartApp.start app

-- model

initialModel : Model
initialModel =
  { width = 5
  , height = 10
  , filled = [(2, 5), (3, 6)]
  , unit = { cells = [(0, 0), (0, 1), (1, 1)], pivot = (1, 0) }
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
  in
    product (range 0 model.width) (range 0 model.height)


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
    cellSize = 56
    cellSpacing = 2
    w = (cellSize + cellSpacing)
    xPos = x cell * w + (if y cell % 2 == 1 then round (0.5 * w) else 0)
    yPos = y cell * (cellSize - 6)
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
    []

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
      (List.map (cellView "yellow") unit.cells) ++ [pivotView unit.pivot]
  in
    div [class "board"]
      (allCellsViews ++ filledCellsViews ++ unitCellsViews model.unit)
