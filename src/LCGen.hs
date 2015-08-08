{-# LANGUAGE CPP #-}

module LCGen where

import Data.Bits ((.&.), Bits, shiftL, shiftR)
import System.Random (RandomGen, genRange, next, split)

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
#else
{-# INLINE build #-}
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif


{-# SPECIALISE INLINE modPow2 :: Int -> Int -> Int #-}
modPow2 :: (Bits a, Num a) => a -> Int -> a
modPow2 n k =
    n .&. ((1 `shiftL` k) - 1)


newtype LCGen = LCGen Int

instance RandomGen LCGen
  where
    genRange _ = (0, 0x7FFF)
    next = nextLCGen
    split = splitLCGen


mkLCGen :: Int -> LCGen
mkLCGen s = LCGen s

nextLCGen :: LCGen -> (Int, LCGen)
nextLCGen (LCGen s) =
    (r, LCGen s')
  where
    r = (s .&. 0x7FFF0000) `shiftR` 16
    s' = (s * 1103515245 + 12345) `modPow2` 32

splitLCGen :: LCGen -> (LCGen, LCGen)
splitLCGen (LCGen s) =
    (LCGen s, LCGen s)


{-# INLINE random #-}
random :: LCGen -> (Int, LCGen)
random g =
    nextLCGen g

{-# INLINE randoms #-}
randoms :: LCGen -> [Int]
randoms g =
    build $ \cons _nil ->
      buildRandoms cons g

{-# SPECIALISE INLINE buildRandoms :: (Int -> [Int] -> [Int]) -> LCGen -> [Int] #-}
buildRandoms :: (Int -> x -> x) -> LCGen -> x
buildRandoms cons =
    loop
  where
    loop g = r `seq` (r `cons` loop g')
      where
        (r, g') = nextLCGen g
