digits :: Int -> [Int]
digits 0 = []
digits n = digits (div n 10) ++ [mod n 10]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

solution :: Int
solution = maximum (filter (isPalindrome . digits) [ x * y | x <- [100..999], y <- [100..999]])