minDef :: Int -> Int -> Int
minDef a b = if a <= b then a else b

minTres :: Int -> Int -> Int -> Int 
minTres a b c = minDef (minDef a b) c
