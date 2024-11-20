module Main (main) where

import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Digite o nome do arquivo: "
    arquivo <- getLine 
    conteudo <- readFile arquivo
    putStrLn (show (length (lines conteudo)))