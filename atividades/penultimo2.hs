penultimo :: [a] -> a
penultimo [] = error "Lista vazia"
penultimo [x] = error "Lista com um elemento"
penultimo xs = head (drop 1 (reverse (xs)))