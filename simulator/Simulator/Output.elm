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

parse : String -> Result String (List Output)
parse outputString =
  decodeString decoder outputString

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

sample =
  """[{"problemId":1234,"seed":1234,"tag":"foo","solution":"iiiiiiimimiiiiiimmimiiiimimimmimimimimmeemmimimiimmmmimmimiimimimmimmimeee mmmimimmimeeemiimiimimimiiiipimiimimmmmeemimeemimimimmmmemimmimmmiiimmmiii piimiiippiimmmeemimiipimmimmipppimmimeemeemimiieemimmmm"}]"""
