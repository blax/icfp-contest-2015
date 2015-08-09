module Util.List where

at : List a -> Int -> Maybe a
at list index =
  List.head (List.drop index list)

product : List a -> List b -> List (a, b)
product xs ys =
  let product' xs ys =
    case xs of
      x::xs' -> (List.map (\y -> (x, y)) ys)::(product' xs' ys)
      []     -> []
  in List.concat (product' xs ys)

range : Int -> Int -> List Int
range a b =
  if a <= b then a :: range (a + 1) b else []

find : (a -> Bool) -> List a -> Maybe a
find predicate =
  List.head << List.filter predicate
