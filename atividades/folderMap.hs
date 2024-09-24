folderSoma :: [Int] -> Int
folderSoma [x] = x
folderSoma (x:xs) = x + folderSoma xs

folderMult :: [Int] -> Int
folderMult [x] = x
folderMult (x:xs) = x * folderMult xs