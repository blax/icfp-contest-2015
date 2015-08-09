module Simulator where

import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time

import Simulator.Simulation as Simulation
import Simulator.Input as Input
import Simulator.Output as Output
import Simulator.Command as Command

type Action = InputChange String | OutputChange String | Submit | Tick

type alias Model =
  { input : String
  , output : String
  , simulation : Maybe Simulation.Model
  }

main =
  let
    ticks = Signal.map (\_ -> Just Tick) (Time.every 200)

    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    models =
      Signal.foldp
        (\(Just action) model -> update action model)
        (update Submit initialModel)
        (Signal.merge actions.signal ticks)
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
    List.head input.units `andThen` \firstUnit ->
    List.head outputs `andThen` \output ->
    Command.decodeList output.solution `andThen` \commands ->
      let attributes =
        { width = input.width
        , height = input.height
        , filled = input.filled
        , units = input.units
        , commands = commands
        }
      in
        Just (Simulation.init attributes)

-- update

update : Action -> Model -> Model
update action model =
  case action of
    InputChange s ->
      { model | input <- s }

    OutputChange s ->
      { model | output <- s }

    Submit ->
      { model | simulation <- toSimulation model }

    Tick ->
      { model | simulation <- Maybe.map Simulation.update model.simulation }

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
