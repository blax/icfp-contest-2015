module Simulator where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Simulator.Simulation as Simulation
import Simulator.Input as Input
import Simulator.Output as Output
import Simulator.Command as Command

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
  { input = Input.sample
  , output = Output.sample
  , simulation = Nothing
  }

toSimulation : Model -> Maybe Simulation.Model
toSimulation model =
  let
    andThen = Maybe.andThen
  in
    Input.parse model.input `andThen` \input ->
    Output.parse model.output `andThen` \outputs ->
    List.head input.units `andThen` \unit ->
    List.tail input.units `andThen` \units ->
    List.head outputs `andThen` \output ->
    Command.decodeList output.solution `andThen` \commands ->

    Just
      { width = input.width
      , height = input.height
      , filled = input.filled
      , unit = unit
      , commands = commands
      }

-- update

update action model =
  case action of
    InputChange s ->
      { model | input <- s }

    OutputChange s ->
      { model | output <- s }

    Submit ->
      { model | simulation <- Maybe.map Simulation.applyCommands (toSimulation model) }

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
          , value model.input
          , onChangeSendAction address InputChange]
          []
      , input
          [ placeholder "Program output"
          , value model.output
          , onChangeSendAction address OutputChange]
          []
      , button [onClick address Submit] [text "Update"]
      , simulationView
      ]
