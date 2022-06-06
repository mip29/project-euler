sumOfSquares :: [Int] -> Int
sumOfSquares = foldr (\x -> (+) (x ^ 2)) 0

squareOfSum :: [Int] -> Int
squareOfSum xs = sum xs^2

solution :: [Int] -> Int
solution xs = squareOfSum xs - sumOfSquares xs