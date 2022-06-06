fibonacci :: [Int]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

sumOfEvenLessThan :: Int -> Int
sumOfEvenLessThan n = sum $ takeWhile (<= n) (filter even fibonacci)

solution :: Int
solution = sumOfEvenLessThan 4000000