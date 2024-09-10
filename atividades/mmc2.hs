mdcDois :: Int -> Int -> Int
mdcDois a 0 = a
mdcDois a b = mdcDois b (a `mod` b)

mmcDois :: Int -> Int -> Int
mmcDois a b = abs (a * b) `div`(mdcDois a b)

mmc :: Int -> Int -> Int -> Int
mmc a b c = mmcDois (mmcDois a b) c

mdc :: Int -> Int -> Int -> Int
mdc a b c = mdcDois (mdcDois a b) c

