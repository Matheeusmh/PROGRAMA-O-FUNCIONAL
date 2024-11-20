module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    lerArquivo

lerArquivo :: IO ()
lerArquivo = do
    putStr "Digite o nome do arquivo: "
    arquivo <- getLine
    conteudo <- readFile arquivo
    let count = contar conteudo 0
    putStrLn ("Número de vogais: " ++ show count)

contar :: String -> Int -> Int
contar [] n = n
contar (x : xs) n
    | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' = contar xs (n + 1)
    | otherwise = contar xs n