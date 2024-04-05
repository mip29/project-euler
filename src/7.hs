data Wheel = Wheel Integer [Integer]

instance Show Wheel where
  show (Wheel n rs) = "Radius: " ++ show n ++ ", " ++ "Spikes: " ++ show rs

roll (Wheel n rs) = [n * k + r | k <- [0 .. 100], r <- rs]

w0 = Wheel 1 [1]

nextSize (Wheel n rs) p = Wheel (p * n) [r2 | k <- [0 .. (p -1)], r <- rs, let r2 = n * k + r, r2 `mod` p /= 0]

mkWheel = foldl nextSize w0

primes :: [Integer]
primes = small ++ large
  where
    1 : p : candidates = roll $ mkWheel small
    small = [2, 3, 5, 7, 11]
    large = p : filter isPrime candidates
    isPrime n = not (any (divides n) (takeWhile (\p -> p * p <= n) large))
    divides n p = n `mod` p == 0

solution = primes !! 10000 -- lists are 0-based