initDef :: [a] -> [a]
initDef [] = error "Lista Vazia"
initDef [_] = []
initDef (x:xs) = x : initDef xs

lastDef :: [a] -> a
lastDef [] = error "Lista Vazia"
lastDef [x] = x
lastDef (_:xs) = lastDef xs

elemDef :: Eq a => a -> [a] -> Bool
elemDef n [] = False
elemDef n (x:xs) |x == n = True
                 |otherwise = elemDef n xs

lengthDef :: [a] -> Int
lengthDef [] = 0
lengthDef (_:xs) = 1 + lengthDef xs

lengthDefCauda :: [a] -> Int
lengthDefCauda [] = 0
lengthDefCauda xs = lengthAux xs 0
    where
        lengthAux [] count = count
        lengthAux (_:xs) count = lengthAux xs (count + 1)

(<!!>) :: [a] -> Int -> a
(x:xs) <!!> 0 = x
(x:xs) <!!> n 
    |n < 0 = error "erro" 
    |n > length xs = error "erro"
    |otherwise = xs <!!> (n - 1) 

takeDef :: Int -> [a] -> [a]
takeDef _ [] = []
takeDef 0 xs = []
takeDef n (x:xs) = x : takeDef (n - 1) xs

dropDef :: Int -> [a] -> [a]
dropDef _ [] = []
dropDef n (x:xs) | n == 0 = x:xs
                 | n < 0 = x:xs
                 | otherwise = dropDef (n - 1) xs

splitDef :: Int -> [a] -> ([a], [a])
splitDef n xs = aux n (remove n 0 xs) xs
    where
        aux n _ ys |n < 0 = ([], ys)
        aux 0 xs ys = (xs, ys)
        aux _ xs [] = (xs, [])
        aux n xs (y:ys) = aux (n - 1) xs ys

        remove _ _ [] = []
        remove n count (x:xs) | n == count = []
                              | otherwise = x : (remove n (count + 1) xs)

infixr 5 <++>
(<++>) :: [a] -> [a] -> [a]
[] <++> ys = ys
(x:xs) <++> ys = x : (xs <++> ys)

concatDef :: [[a]] -> [a]
concatDef [] = []
concatDef (xs:xss) = xs ++ concatDef xss

reverseDef :: [a] -> [a]
reverseDef [] = []
reverseDef (x:xs) = reverseDef xs ++ [x]

zipDef :: [a] -> [b] -> [(a, b)]
zipDef [] ys = []
zipDef xs [] = []
zipDef (x:xs) (y:ys) = (x, y) : (zipDef xs ys)

multa :: [Int] -> Int -> [Int]
multa xs n = [x * n | x <- xs]

justNeg :: [Int] -> [Int]
justNeg xs = [x | x <- xs, x < 0]

distancias :: [(Float, Float)] -> [Float]
distancias xs = [sqrt ((x * x) + (y * y)) | (x, y)<- xs]

--25--
biggerThan :: [Float] -> [Float] 
biggerThan xs = [x | x <- xs, x > media xs]
    where
        media xs = sum xs / fromIntegral (length xs)

--26--
maximos :: [(Double, Double)] -> [Double]
maximos xs = [if a > b then a else b | (a, b) <- xs]

