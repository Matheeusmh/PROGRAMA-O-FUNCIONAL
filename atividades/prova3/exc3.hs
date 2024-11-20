module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

prompt mensagem = do {putStr mensagem; readLn :: IO Double}

celsius :: Double -> Double
celsius f = (5 / 9) * (f - 32)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    graus <- prompt "Digite os graus em Fahrenheit: "

    let c = celsius graus in putStrLn ("Grau em Celsius: " ++ show c)

