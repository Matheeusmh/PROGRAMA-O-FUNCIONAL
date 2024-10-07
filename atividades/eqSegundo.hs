raizeslet :: (Double, Double, Double) -> (Double, Double)
raizeslet (a, b, c) = 
    let delta =  b * b - 4 * a * c 
    in if delta < 0 then error "Discriminante negativa"
    else ((-b + sqrt delta) / (2 * a), ((-b - sqrt delta) / (2 * a)))

raizesWhere :: (Double, Double, Double) -> (Double, Double)
raizesWhere (a, b, c) = if delta < 0 then error "Discriminante Negativa!" else ((-b + sqrt delta) / (2 * a), (-b - sqrt delta) / (2 * a))
    where 
        delta = b ^ 2 - 4 * a * c