mdc2 :: Int -> Int -> Int
mdc2 a 0 = a
mdc2 a b = mdc2 b (mod a b)

mdc :: Int -> Int -> Int -> Int
mdc a b c = mdc2 (mdc2 a b) c

mmc2 :: Int -> Int -> Int 
mmc2 a b = abs (a * b) `div` (mdc2 a b)

mmc :: Int -> Int -> Int -> Int
mmc a b c = mmc2 (mmc2 a b) c
