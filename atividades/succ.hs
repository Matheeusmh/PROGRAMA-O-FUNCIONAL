succe :: Num a => [a] -> a
succe [] = 0
succe [x] = 0
succe (_:(x:xs)) = x

succel :: Num a => [a] -> a
succel xs = head (drop 1 xs)