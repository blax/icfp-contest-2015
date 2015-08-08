module Simulator.Input where

import Json.Decode as Json exposing (..)

import Simulator.Cell exposing (Cell)
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

parse : String -> Result String Input
parse inputString =
  decodeString decoder inputString

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
  object2 (,)
    ("x" := int)
    ("y" := int)

unit : Decoder Unit
unit =
  object2 Unit
    ("members" := list cell)
    ("pivot" := cell)

sample =
  """{"height":10,"width":10,"sourceSeeds":[0,13120,18588,31026,7610,25460,23256,19086,24334,22079,9816,8466,3703,13185,26906,16903,24524,9536,11993,21728,2860,13859,21458,15379,10919,7082,26708,8123,18093,26670,16650,1519,15671,24732,16393,5343,28599,29169,8856,23220,25536,629,24513,14118,17013,6839,25499,17114,25267,8780],"units":[{"members":[{"x":0,"y":0}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0}],"pivot":{"x":1,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0},{"x":0,"y":1}],"pivot":{"x":0,"y":0}},{"members":[{"x":0,"y":0},{"x":1,"y":0},{"x":1,"y":1}],"pivot":{"x":1,"y":0}}],"id":6,"filled":[],"sourceLength":150}"""
