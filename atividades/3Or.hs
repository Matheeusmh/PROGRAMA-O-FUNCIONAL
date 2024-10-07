infixr 2 ||-
??

infixr 2 ||--
(||--) :: Bool -> Bool -> Bool
b ||-- a 
    |b == True = True
    |otherwise = a

infixr 2 ||---
(||---) :: Bool -> Bool -> Bool
False ||--- False = False
False ||--- True = True
True ||--- False = True
True ||--- True = True
