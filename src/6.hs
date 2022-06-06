sumOfSquares :: [Int] -> Int
sumOfSquares = foldr (\x -> (+) (x ^ 2)) 0

squareOfSum :: [Int] -> Int
squareOfSum xs = sum xs^2

sumSquareDifference :: [Int] -> Int
sumSquareDifference xs = squareOfSum xs - sumOfSquares xs

solution :: Int
solution = sumSquareDifference [1..100]