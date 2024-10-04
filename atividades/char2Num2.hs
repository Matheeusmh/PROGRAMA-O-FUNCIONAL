char2Num :: Char -> Int
char2Num c | fromEnum c < 48 || fromEnum c > 57 = 0
           | otherwise = fromEnum c - fromEnum '0'