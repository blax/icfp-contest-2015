module Simulator.Random (init, next, take) where

type LCGen = LCGen Int

init : Int -> LCGen
init s = LCGen s

take : Int -> LCGen -> (List Int, LCGen)
take n g =
  if n > 0 then
    let
      (r, g') = next g
      (tail, g'') = take (n - 1) g'
    in
      (r :: tail, g'')
  else
    ([], g)

next : LCGen -> (Int, LCGen)
next (LCGen seed) =
  let
    increment =
      12345

    (m1, m2, m3, m4) =
      (129749, 7, 5, 3^5) -- factorization of 1103515245

    seedTimesMultiplier =
      ((((((seed * m1) % 2^32) * m2) % 2^32) * m3 % 2^32) * m4) % 2^32

    seed' =
      (seedTimesMultiplier + increment) % 2^32

    r =
      (seed // 2^16) % 2^15
  in
    (r, LCGen seed')
