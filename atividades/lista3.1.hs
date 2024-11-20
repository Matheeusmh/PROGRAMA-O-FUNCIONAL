import Data.List(delete)

--34
duplicar :: [a] -> [a]
duplicar [] = []
duplicar (x:xs) = x : x : duplicar xs

--35
replique :: [a] -> Int -> [a]
replique [] n = []
replique (x:xs) n = (aux x n) ++ replique xs n
    where
        aux _ 0 = []
        aux a count = a : aux a (count-1)

--38
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (delete x xs)]