ordenaTripla :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)

ordenaTripla (a, b, c) 
 | a > b && a > c && b > c = (c, b, a)
 | b > a && b > c && a > c = (c, a, b)
 | a > b && a > c && b < c = (b, c, a)
 | b > a && b > c && a < c = (a, c, b)
 | c > a && c > b && a > b = (b, a, c)
 | otherwise = (a, b, c)
