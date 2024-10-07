soma2 :: Num a => [a] -> a
soma2 [] = 0
soma2 [x] = x
soma2 (x:y:_) = x + y

soma2l :: Num a => [a] -> a
soma2l xs = sum (take 2 xs)