find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x : xs) = if f x then Just x else find f xs

primeFactors :: Int -> [Int]
primeFactors n = case getSmallestDivisor of
  Nothing -> [n]
  Just x -> x : primeFactors (div n x)
  where
    getSmallestDivisor = find (\x -> n `mod` x == 0) [2 .. n - 1]

solution :: Int
solution = last (primeFactors 600851475143)