productDef :: Num a => [a] -> a 
productDef [] = 1
productDef (x:xs) = x * productDef xs
