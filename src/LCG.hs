module LCG where

lcg seed = (extract seed):lcg r where
  r = (seed * multiplier + increment) `mod` 2^32
  extract x = (x `div` 2^16) `mod` 2^15
  multiplier = 1103515245
  increment = 12345
