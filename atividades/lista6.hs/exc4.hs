module Main (main) where 

import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStr "Digite o nome do arquivo: "
    arquivo <- getLine
    putStr "Digite um char: "
    c <- getChar 
    conteudo <- readFile arquivo

    let count = verificaRep conteudo c 0

    putStr ("A letra " ++ show c ++ " se repete: " ++ show count ++ " vezes")

verificaRep :: String -> Char -> Int -> Int
verificaRep [] _ count = count
verificaRep (x:xs) c count | x == c = verificaRep xs c (count + 1)
                           | otherwise = verificaRep xs c count

