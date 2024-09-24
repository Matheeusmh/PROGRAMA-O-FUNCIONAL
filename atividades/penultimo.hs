penultimo :: [a] -> a
penultimo xs 
 |length xs == 0 = error "Vazio"
 |length xs == 1 = error "Um elemento"
 |otherwise = head (drop 1 (reverse xs))
