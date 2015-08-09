module Simulator.Simulation (Model, update, view, updateAll) where

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

update : Command -> Model -> Model
update command model =
  case model.unit of
    Nothing ->
      case model.units of
        (head :: tail) ->
          update command
            { model | unit <- Just (centerUnit model.width head), units <- tail }

        [] ->
          model -- FIXME handle game over properly

    Just unit ->
      { model | unit <- Just (commandUnit command unit) }

updateAll : Model -> Model
updateAll model =
  List.foldl update model model.commands

-- view

view address model =
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
