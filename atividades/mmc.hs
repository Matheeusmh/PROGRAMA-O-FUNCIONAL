mmc :: Int -> Int -> Int -> Int
mmcDois :: Int -> Int -> Int
mmcDois a b = abs (a * b) `div` (gcd a b)
mmc a b c = mmcDois (mmcDois a b) c
