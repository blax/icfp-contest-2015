module Simulator.Command where

type Move = E | W | SE | SW

type Rotation = CW | CCW

type Command = Move Move | Rotate Rotation

type alias CommandCode = Char

decode : CommandCode -> Maybe Command
decode code =
  let
    mapping =
      [ (['p', '\'', '!', '.', '0', '3'], Move W)
      , (['b', 'c', 'e', 'f', 'y', '2'],  Move E)
      , (['a', 'g', 'h', 'i', 'j', '4'],  Move SW)
      , (['l', 'm', 'n', 'o', ' ', '5'],  Move SE)
      , (['d', 'q', 'r', 'v', 'z', '1'],  Rotate CW)
      , (['k', 's', 't', 'u', 'w', 'x'],  Rotate CCW)
      ]

    first : (a -> Bool) -> List a -> Maybe a
    first predicate =
      List.head << List.filter predicate

    maybeMappingRow =
      first (\(chars, command) -> List.member code chars) mapping

    maybeCommand =
      Maybe.map snd maybeMappingRow
 in
    maybeCommand

decodeList : List CommandCode -> Maybe (List Command)
decodeList codes =
  sequence (List.map decode codes)

sequence : List (Maybe a) -> Maybe (List a)
sequence list =
  let
    step maybe results =
      case results of
        Just results ->
          case maybe of
            Just value -> Just (value :: results)
            Nothing -> Nothing

        Nothing ->
          Nothing
  in
    List.foldr step (Just []) list
