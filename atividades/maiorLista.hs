maiorLista :: [Int] -> Int
maiorLista [x] = x
maiorLista (x:xs) = if x > maiorLista xs then x else maiorLista xs