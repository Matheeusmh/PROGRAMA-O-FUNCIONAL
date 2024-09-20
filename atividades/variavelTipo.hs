head1 :: [a] -> a
head1 xs = xs !! 0

tail1 :: [a] -> [a]
tail1 xs = drop 1 xs
