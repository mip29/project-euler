isMultipleOf3Or5 :: Int -> Bool
isMultipleOf3Or5 n = mod n 3 == 0 || mod n 5 == 0

sumBelow :: Int -> Int
sumBelow n = sum $ filter isMultipleOf3Or5 [0..n-1]

solution :: Int
solution = sumBelow 1000