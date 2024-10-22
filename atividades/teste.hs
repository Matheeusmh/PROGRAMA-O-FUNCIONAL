abreviaTudo :: [String] -> [String]
abreviaTudo [] = []
abreviaTudo (x:xs) = abrevia x : abreviaTudo xs
    where
        abrevia :: String -> String
        abrevia [] = []
        abrevia (x:xs) = x : ". " ++ sobrenome (reverse xs)
            where
                sobrenome (' ':_) = []
                sobrenome (x:xs) = sobrenome xs ++ [x] 

tobit :: Int -> [Int]
tobit 0 = [0]
tobit 1 = [1]
tobit x | mod x 2 == 0 = tobit (div x 2) ++ [0]
        | otherwise = tobit (div x 2) ++ [1]