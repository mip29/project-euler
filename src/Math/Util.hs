module Math.Util
  ( fibonacci,
    factors,
    primes,
    lcmm,
  )
where

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- only works on ordered lists
minus :: (Ord a) => [a] -> [a] -> [a]
minus (x : xs) (y : ys) = case compare x y of
  LT -> x : minus xs (y : ys)
  EQ -> minus xs ys
  GT -> minus (x : xs) ys
minus xs _ = xs

factors :: Integer -> [Integer]
factors n = f n (head primes) (tail primes)
  where
    f n p ps
      | n < 2 = []
      | n < p ^ 2 = [n]
      | n `mod` p == 0 = p : f (n `div` p) p ps
      | otherwise = f n (head ps) (tail ps)

primes :: [Integer]
primes = 2 : filter (\n -> head (factors n) == n) [3, 5 ..]

lcmm :: (Integral a, Foldable t) => t a -> a
lcmm = foldr lcm 1