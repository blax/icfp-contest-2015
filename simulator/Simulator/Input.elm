module Simulator.Input where

import Json.Decode as Json exposing (..)

import Simulator.Cell exposing (Cell, fromInputCell, InputCell(..))
import Simulator.Unit exposing (Unit)

type alias Input =
  { id: Int
  , width: Int
  , height: Int
  , filled: List Cell
  , units: List Unit
  , sourceSeeds: List Int
  , sourceLength: Int
  }

parse : String -> Maybe Input
parse inputString =
  Result.toMaybe (decodeString decoder inputString)

decoder : Decoder Input
decoder =
  object7 Input
    ("id" := int)
    ("width" := int)
    ("height" := int)
    ("filled" := list cell)
    ("units" := list unit)
    ("sourceSeeds" := list int)
    ("sourceLength" := int)

cell : Decoder Cell
cell =
  let
    convert x y = fromInputCell (InputCell (x, y))
  in
    object2 convert
      ("x" := int)
      ("y" := int)

unit : Decoder Unit
unit =
  object2 Unit
    ("members" := list cell)
    ("pivot" := cell)
