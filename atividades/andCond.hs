infixr 3 <&&>
(<&&>) :: Bool -> Bool -> Bool
a <&&> b 
    |a == True && b == True = True
    |otherwise = False

infixr 3 <&&&>
(<&&&>) :: Bool -> Bool -> Bool
a <&&&> b 
    |a == True = b
    |a == False = a