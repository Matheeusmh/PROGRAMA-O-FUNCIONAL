infixr 2 ||---
(||---) :: Bool -> Bool -> Bool
False ||--- False = False
False ||--- True = True
True ||--- False = True
True ||--- True = True

from :: Int -> [Int]
from n = n:from (n+1)