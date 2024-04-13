module Math.Util where

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

lcmm :: (Integral a, Foldable t) => t a -> a
lcmm = foldr lcm 1