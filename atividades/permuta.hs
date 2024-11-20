import Data.List (delete)

permuta :: Eq a => [a] -> [[a]]
permuta [] = [[]]
permuta xs = [x:ys | x <- xs, ys <- permuta (delete x xs)]