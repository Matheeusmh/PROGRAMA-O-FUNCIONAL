andDef :: [Bool]->Bool
andDef [] = True
andDef (x:xs) = x && andDef xs