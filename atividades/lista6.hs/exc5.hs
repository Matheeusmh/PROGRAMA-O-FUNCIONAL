module Main (main) where 

import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    putStr "Digite o nome do primeiro arquivo: "
    arquivo1 <- getLine
    putStr "Digite o nome do segundo arquivo: "
    arquivo2 <- getLine 

    conteudo1 <- readFile arquivo1 
    conteudo2 <- readFile arquivo2 

    writeFile "arquivo3.txt" (conteudo1 ++ conteudo2)