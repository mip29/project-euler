-- This is Euclid's formula for calculating pythegorean triples.
pythagoreanTriples = [[m ^ 2 - n ^ 2, 2 * m * n, m ^ 2 + n ^ 2] | n <- [1 .. 400], m <- [n + 1 .. 400]]

solution = product $ head $ filter (\as -> sum as == 1000) pythagoreanTriples