import Data.Char(isLower, toUpper)

bissexto :: Int -> Bool
bissexto ano |mod ano 400 == 0 = True 
             |mod ano 4 == 0 && mod ano 100 /= 0 = True
             |otherwise = False

intercectaX :: (Float, Float) -> (Float, Float)
intercectaX (a, b) = (((-b)/a), 0.0)

interceccaoRetas :: (Float, Float) -> (Float, Float) -> (Float, Float)
interceccaoRetas (a1, b1) (a2, b2) = (x, y)
    where
        x = (b2 - b1) / (a1 - a2)
        y = a1 * x + b1

toMaiusculo :: String -> String
toMaiusculo [] = []
toMaiusculo (letra:palavra) | isLower letra = toUpper letra : toMaiusculo palavra 
                            | otherwise = letra : toMaiusculo palavra

isPalindromo :: [Char] -> Bool 
isPalindromo palavra | reverse palavra == palavra = True 
                     | otherwise = False

fibonacci :: Int -> Int 
fibonacci 0 = 0 
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

numRaizes :: Float -> Float -> Float -> Int 
numRaizes a b c  |delta == 0 = 1
                 |delta < 0 = 0
                 |otherwise = 2
                 where 
                    delta = b ^ 2 - 4 * a * c

mdcDois :: Int -> Int -> Int 
mdcDois a b |b == 0 = a
            |otherwise = mdcDois b (mod a b)

mdc :: Int -> Int -> Int -> Int
mdc a b c = mdcDois a (mdcDois b c)

mmcDois :: Int -> Int -> Int 
mmcDois a b = div (abs (a * b)) (mdcDois a b)

mmc :: Int -> Int -> Int -> Int 
mmc a b c = mmcDois a (mmcDois b c)

init' :: [a] -> [a]
init' [x] = []
init' (x:xs) = x : (init' xs)

last' :: [a] -> a 
last' [x] = x 
last' (_:xs) = last' xs

elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False 
elem' n (x:xs) = n == x || (elem' n xs)

length' :: [a] -> Int 
length' [] = 0 
length' (x:xs) = 1 + (length' xs)


infixl 9 !!! 
(!!!) :: [a] -> Int -> a
(x:xs) !!! n | n == 0 = x 
(x:xs) !!! n = xs !!! (n - 1)

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n _ |n <= 0 = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n xs | n <= 0 = xs
drop' n (_:xs) = drop' (n - 1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' n xs |n <= 0 = ([], xs)
splitAt' n (x:xs) = (x : x', x'')
    where
        (x', x'') = splitAt' (n - 1) xs

infixr 5 +++
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys 
(x:xs) +++ ys = x : (xs +++ ys)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

multRecursivo :: Int -> Int -> Int 
multRecursivo _ 0 = 0
multRecursivo 0 _ = 0
multRecursivo a b |b < 0 = multRecursivo b a
                  |b == 1 = a
                  |otherwise = a + multRecursivo a (b - 1)

divRecursivo :: Float -> Float -> Float 
divRecursivo _ 0 = error "Nao pode isso, meno"
divRecursivo 0 _ = 0
divRecursivo a b = aux a b 0
    where 
        aux a b count |a < b = count
                      |otherwise = aux (a - b) b (count + 1)