import Math.Util (fibonacci)

solution = sum $ takeWhile (<= 4_000_000) $ filter even fibonacci