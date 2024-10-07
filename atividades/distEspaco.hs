dist :: Floating a => (a, a, a) -> (a, a, a) -> a
dist (x1, y1, z1) (x2, y2, z2) = sqrt somaQuad
    where
        somaQuad = (x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2