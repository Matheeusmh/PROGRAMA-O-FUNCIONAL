--1
--2
foldn :: (a -> a) -> a -> Int -> a
foldn f e 0 = e
foldn f e n = f (foldn f e (n-1))

replica :: Int -> a -> [a]
replica 0 _ = []
replica n v = foldn (v :) [] n

--3
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr consF []
   where
      consF x acc = f x : acc

--5
pertence :: Eq a => a -> [a] -> Bool
pertence v xs = foldl (||) False (matches xs v)
   where
      matches xs v = map (== v) xs
   
--6-a
todos :: (a -> Bool) -> [a] -> Bool
todos p xs = foldr (&&) True (aux p xs)
   where 
      aux p xs = map p xs

positivos :: [Int] -> Bool
positivos xs = todos (>= 0) xs

--b
algum :: (a->Bool) -> [a] -> Bool
algum p xs = foldl (||) False (aux p xs) 
   where
      aux p xs = map p xs

membro :: Eq a => a -> [a] -> Bool
membro v xs = algum (== v) xs

--5--b

remdups :: Eq a => [a] -> [a]
remdups [] = []
remdups (x:xs) = x : remdups (filter (/= x) xs)

--2--a

foldn2 :: (a -> a) -> a -> Int -> a
foldn2 f e 0 = e
foldn2 f e n = f (foldn2 f e (n-1))

replica2 :: Int -> a -> [a]
replica2 0 _ = []
replica2 n x = foldn2 (x :) [] n

dupS :: String -> String
dupS xs = concatMap (replica2 2) xs






--5--b
remdups2 :: Eq a => [a] -> [a]
remdups2 [] = []
remdups2 (x:xs) = x : filter (/= x) (remdups xs)
