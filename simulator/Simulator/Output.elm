module Simulator.Output where

import Json.Decode as Json exposing (..)
import String
import Simulator.Command exposing (CommandCode)

type alias Output =
  { problemId: Int
  , seed: Int
  , tag: String
  , solution: List CommandCode
  }

parse : String -> Maybe (List Output)
parse outputString =
  Result.toMaybe (decodeString decoder outputString)

decoder : Decoder (List Output)
decoder =
  list entry

entry : Decoder Output
entry =
  object4 Output
    ("problemId" := int)
    ("seed" := int)
    ("tag" := string)
    ("solution" := commandCodes)

commandCodes : Decoder (List CommandCode)
commandCodes =
  map (String.toList) string
  -- FIXME skip \t, \n, \r
