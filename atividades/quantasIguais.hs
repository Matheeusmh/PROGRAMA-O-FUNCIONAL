quantasIguais :: Integer -> Integer -> Integer -> Integer

quantasIguais a b c
 |a == b && b == c = 3
 |a == b || b == c || a == c = 2
 |otherwise = 0
