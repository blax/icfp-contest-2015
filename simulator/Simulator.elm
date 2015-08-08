module Simulator where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)

type alias Cell = { x : Int, y : Int }

type alias Model =
  { width : Int
  , height : Int
  , filled : List Cell
  }

type alias Action = ()

main =
  let
    app = { model = initialModel, view = view, update = update }
  in
    StartApp.start app

initialModel : Model
initialModel =
  { width = 5
  , height = 10
  , filled = [{x = 2, y = 5}, {x = 3, y = 6}]
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

    toCell (x, y) =
      { x = x, y = y }

    pairs = product (range 0 model.width) (range 0 model.height)
  in
    List.map toCell pairs

update : Action -> Model -> Model
update action model =
  model

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
    x = cell.x * w + (if cell.y % 2 == 1 then round (0.5 * w) else 0)
    y = cell.y * (cellSize - 6)
  in
    [ ("left", (toString x) ++ "px")
    , ("top",  (toString y) ++ "px")
    ]

cellView : String -> Cell -> Html
cellView color cell =
  div
    [ class ("hexagon hexagon-56 hexagon-" ++ color)
    , style ([("position", "absolute")] ++ (cellCssPosition cell))
    ]
    []

boardView : Model -> Html
boardView model =
  div [class "board"]
    ((List.map (cellView "gray") (allCells model)) ++ (List.map (cellView "yellow") (model.filled)))
