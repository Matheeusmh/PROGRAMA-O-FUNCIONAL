halveDef :: [a] -> ([a],[a])
halveDef xs = (take metade xs, drop metade xs)
    where
        metade = (length xs) `div` 2