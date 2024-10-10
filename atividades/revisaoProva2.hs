import Data.Char(toUpper, isLower)

maiuscula :: [Char] -> [Char]
maiuscula [] = []
maiuscula (x:xs) | isLower x = toUpper x : maiuscula xs
                 | otherwise = x : maiuscula xs

char2Num :: Char -> Int
char2Num c | fromEnum c < 48 || fromEnum c > 59 = 0
           | otherwise =  fromEnum c - fromEnum '0'


mediaTres :: Integer -> Integer -> Integer -> Double
mediaTres a b c = fromIntegral (a + b + c) / fromIntegral 3

quantosAcimaMedia :: Integer -> Integer -> Integer -> Integer
quantosAcimaMedia a b c = isMaior a + isMaior b + isMaior c
    where
        isMaior n |fromIntegral n > mediaTres a b c = 1
                  |otherwise = 0

numeroRaizes :: Double -> Double -> Double -> Integer
numeroRaizes a b c | delta == 0 = 1
                   | delta < 0 = 0
                   | otherwise = 2
                   where
                    delta = b * b - 4 * a * c

entre :: Integer -> Integer -> Integer -> Bool
entre m n p |m > p = entre p n m 
            |otherwise = n > m && n < p

numeroDoMeio :: Integer -> Integer -> Integer -> Integer
numeroDoMeio a b c |entre a b c = b 
                   |entre a c b = c
                   |entre b a c = a
                   |entre b c a = a
                   |otherwise = c

quantasIguais :: Integer -> Integer -> Integer -> Integer
quantasIguais a b c | a == b && b == c = 3
                    | a == c || a == b || b == c = 2
                    | otherwise = 0

maxTresOcorrencias :: Int -> Int -> Int -> (Int, Int)
maxTresOcorrencias a b c = (maior a b c, repete [a, b, c] 0)
    where 
        maior a b c |a > b && a > c = a
                    |b > a && b > c = b
                    |otherwise = c
        
        repete [] count = count
        repete (x:xs) count |maior a b c == x = repete xs (count + 1)
                            |otherwise = repete xs count

ordenaTripla :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
ordenaTripla (a, b, c) | a > b && a > c = ordenaTripla (b, c, a)
                       | b > c = ordenaTripla (a, c, b)
                       | a > b = ordenaTripla (b, a, c)
                       |otherwise = (a, b, c)

intersectaX :: (Float, Float) -> Float
intersectaX (a, b) = (-b) / a

intersectaRetas :: (Float, Float) -> (Float, Float) -> (Float, Float)
intersectaRetas (a1, b1) (a2, b2) = (x, y)
    where
        x = (b2 - b1) / (a1 - a2)
        y = a1 * x + b1

toSegundos :: Num a => (a, a, a) -> a
toSegundos (a, b, c) = (a * 3600) + (b * 60) + c

bissexto :: Int -> Bool
bissexto ano | mod ano 400 == 0 = True 
             | mod ano 4 == 0 && mod ano 100 /= 0 = True 
             | otherwise = False

interseccaoRETAS :: (Float, Float) -> (Float, Float) -> (Float, Float)
interseccaoRETAS (a1, b1) (a2, b2) = (x, y)
    where
        x = (b2 - b1) / (a1 - a2)
        y = a1 * x + b1

mdcDois :: Int -> Int -> Int
mdcDois a b |b == 0 = a 
            |otherwise = mdc b (mod a b)

mdc :: Int -> Int -> Int -> Int 
mdc a b c = mdcDois a (mdcDois b c)

mmcDois :: Int -> Int -> Int 
mmcDois a b = div (abs (a * b)) (mdcDois a b)

mmc :: Int -> Int -> Int -> Int 
mmc a b c = mmcDois a (mmcDois b c)