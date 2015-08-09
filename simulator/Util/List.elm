module Util.List where

at : List a -> Int -> Maybe a
at list index =
  List.head (List.drop index list)
