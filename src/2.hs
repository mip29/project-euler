fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

solution :: Int
solution = sum [ x | x <- takeWhile (<= 4000000) fib, even x]