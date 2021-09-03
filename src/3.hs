find f [] = Nothing
find f (x:xs) = if f x then Just x else find f xs

primeFactors n = let result = getSmallestDivisor in case result of
    Nothing -> [n]
    Just x -> x : primeFactors (div n x)
    where 
        getSmallestDivisor = find (\x -> mod n x == 0) [2..n-1]
    
solution = last (primeFactors 600851475143)

main = print solution