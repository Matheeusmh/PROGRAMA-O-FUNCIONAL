mdcDois :: Int -> Int -> Int
mdcDois a b
    |b == 0 = a
    |b > 0 = mdcDois b (mod a b)
    |otherwise = mdcDois a (-b)

mdc :: Int -> Int -> Int -> Int
mdc a b c = mdcDois a (mdcDois b c)

mmcDois :: Int -> Int -> Int
mmcDois a b = div (abs (a * b)) (mdcDois a b)

mmc :: Int -> Int -> Int -> Int
mmc a b c = mmcDois a (mmcDois b c)