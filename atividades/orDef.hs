orDef :: [Bool]->Bool
orDef [] = False
orDef (x:xs) = x || orDef xs