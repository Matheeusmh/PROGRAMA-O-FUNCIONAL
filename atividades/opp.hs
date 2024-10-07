opp :: (Int,(Int,Int)) -> Int
opp (1, (x, y)) = x + y
opp (2, (x, y)) = x - y
opp (_, (x, y)) = 0