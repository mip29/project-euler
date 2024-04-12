module Math.Util where

lcmm :: (Integral a, Foldable t) => t a -> a
lcmm = foldr lcm 1