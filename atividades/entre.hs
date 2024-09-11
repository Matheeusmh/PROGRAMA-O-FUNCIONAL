entre :: Integer -> Integer -> Integer -> Bool
entre m n p = if n > m && n < p then True else False

numeroDoMeio :: Integer -> Integer -> Integer -> Integer
numeroDoMeio a b c = if (entre a b c) then b 
 else if (entre a c b) then c else a

