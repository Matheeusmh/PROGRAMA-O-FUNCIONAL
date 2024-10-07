infixr 2 ||-
??

infixr 2 ||--
(||--) :: Bool -> Bool -> Bool
??

infixr 2 ||---
(||---) :: Bool -> Bool -> Bool
False ||--- False = False
False ||--- True = True
True ||--- False = True
True ||--- True = True
