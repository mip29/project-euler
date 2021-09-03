fib = 0 : 1 : zipWith (+) fib (tail fib)

solution = sum [ x | x <- takeWhile (<= 4000000) fib, even x]

main = print solution 