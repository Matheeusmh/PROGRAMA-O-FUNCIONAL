import Data.List (delete)

permuta :: Eq a => [a] -> [[a]]
permuta [] = [[]]
permuta xs = [x:ys | x <- xs, ys <- permuta (delete x xs)]
main :: IO ()
main = do
    putStrLn "Digite uma lista de elementos separados por espaco:"
    input <- getLine
    let elements = words input
    let permutations = permuta elements
    putStrLn "As permutacoes sao:"
    mapM_ print permutations