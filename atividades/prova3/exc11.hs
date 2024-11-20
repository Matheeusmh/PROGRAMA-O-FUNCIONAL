module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering

    putStr "Digite uma sequencia numerica: "
    calcularMedia 0 0

calcularMedia :: Double -> Int -> IO ()
calcularMedia soma count = do 
    valor <- readLn :: IO Double
    
    if valor >= 0 
        then do
            let novaSoma = valor + soma
            calcularMedia novaSoma (count + 1)
        else do
            putStrLn ("A media eh: " ++ show (soma / fromIntegral count))