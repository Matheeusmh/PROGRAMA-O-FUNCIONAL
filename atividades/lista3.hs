prodMn :: Int -> Int -> Int
prodMn m n
    |m > n = prodMn n m 
    |m == n = m
    |otherwise = m * prodMn (m + 1) n

sumQuad :: Int -> Int
sumQuad 0 = 0 * 0
sumQuad x = (x * x) + sumQuad(x - 1)

sumFats :: Int -> Int
sumFats 0 = 1
sumFats 1 = 1
sumFats x = fatorial x + sumFats (x - 1)
    where
        fatorial :: Int -> Int
        fatorial 0 = 1
        fatorial 1 = 1
        fatorial x = x * fatorial (x - 1)

duplicate :: Int -> String -> String
duplicate 0 xs = []
duplicate n xs = xs ++ duplicate (n - 1) xs

--exc 3--

fat :: Integer -> Integer
fat 0 = 1
fat 1 = 1
fat n = fat' n 1
    where  
        fat' 1 aux = aux
        fat' n aux = fat' (n - 1) (n * aux)

reverseDef :: [a] -> [a]
reverseDef [] = error "Lista Vazia"
reverseDef xs = reverse' xs []
    where
        reverse' [] acc = acc
        reverse' (x:xs) acc = reverse' xs (acc ++ [x])

retornaSup :: Int -> [Int] -> Int
retornaSup n xs = retornaSupAux n xs 0
    where
        retornaSupAux n [] count = count
        retornaSupAux n (x:xs) count |x > n = retornaSupAux n xs (count + 1)
                                     |otherwise = retornaSupAux n xs count

retornaListaSup :: Int -> [Int] -> [Int]
retornaListaSup n xs = retornaListaSupAux n xs
    where
        retornaListaSupAux n [] = []
        retornaListaSupAux n (x:xs) |x > n = x : retornaListaSupAux n xs 
                                    |otherwise = retornaListaSupAux n xs 

diferentes :: Eq a => [a] -> Bool
diferentes [] = True
diferentes (x:xs) = not (x `elem` xs) && diferentes xs
        
retornaSupNovo :: Int -> [Int] -> Int
retornaSupNovo n xs =  count [x | x <- xs, x > n]
    where
        count [] = 0
        count (x:xs) = 1 + count xs

mult_listas :: [Int] -> [Int] -> [[Int]]
mult_listas xs ys = [[x * y | y <- ys] | x <- xs]

diferentesA :: Eq a => [a] -> Bool
diferentesA [] = True
diferentesA (x:xs) = not (x `elem` xs) && diferentesA xs

intersecao :: Eq t => [t] -> [t] -> [t]
intersecao as bs = [b | b <- bs, (elem b as)]

nonAbs :: [Int] -> [Int]
nonAbs xs = [x | x <- xs, x < 0]

distancias :: [(Double, Double)] -> [Double] 
distancias xs = [ sqrt(((x - 0) ^ 2) + ((y - 0) ^ 2))| (x, y) <-xs]

tabuada :: Int -> [(Int, Int, Int)] 
tabuada n = [(n, y, n * y) | y <-aux]
    where
        aux = [1..10]

produto :: Num a => [a] -> a
produto xs = foldl1 (*) xs

prefixo :: Eq a => [a] -> [a] -> Bool
prefixo xs ys = xs == elimina ys    
    where
        elimina ys = reverse (drop ((length ys) - (length xs)) (reverse ys))

subSeq :: Eq a => [a] -> [a] -> Bool
subSeq xs ys = xs == aux xs ys 
 where
    aux xs [] = False
    