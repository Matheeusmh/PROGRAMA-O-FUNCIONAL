fib :: Int -> Int
fib a
    |a == 0 = 1
    |a == 1 = 1
    |otherwise = fib(a - 1) + fib(a - 2)