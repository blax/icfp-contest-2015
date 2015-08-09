module Util.Maybe where

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
