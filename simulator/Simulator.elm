module Simulator where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Simulator.Simulation as Simulation
import Simulator.Input as Input
import Simulator.Output as Output

type Action = InputChange String | OutputChange String | Submit

type alias Model =
  { input : String
  , output : String
  , simulation : Maybe Simulation.Model
  }

main =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    models =
      Signal.foldp
        (\(Just action) model -> update action model)
        initialModel
        actions.signal
  in
    Signal.map (view address) models

-- model

initialModel =
  { input = ""
  , output = ""
  , simulation = Nothing
  }

toSimulation : Model -> Maybe Simulation.Model
toSimulation model =
  let
    input : Result String Input.Input
    input =
      Input.parse model.input

    output : Result String (List Output.Output)
    output =
      Output.parse model.output

    toSimulationModel : Input.Input -> List Output.Output -> Simulation.Model
    toSimulationModel input outputs =
      let
        (unit, units) = (\(head :: tail) -> (head, tail)) input.units -- hackish
      in
        { width = input.width
        , height = input.height
        , filled = input.filled
        , unit = unit
        }
  in
    Result.toMaybe (Result.map2 toSimulationModel input output)

-- update

update action model =
  case action of
    InputChange s ->
      { model | input <- s }

    OutputChange s ->
      { model | output <- s }

    Submit ->
      { model | simulation <- toSimulation model }

-- view

onChangeSendAction address valueToAction =
   on "change" targetValue (\v -> Signal.message address (valueToAction v))

view address model =
  let
    simulationView =
      case model.simulation of
        Just simulation -> Simulation.view address simulation
        Nothing -> text "..."
  in
    div []
      [ input
          [ placeholder "Program input"
          , onChangeSendAction address InputChange]
          []
      , input
          [ placeholder "Program output"
          , onChangeSendAction address OutputChange]
          []
      , button [onClick address Submit] [text "Update"]
      , simulationView
      ]
