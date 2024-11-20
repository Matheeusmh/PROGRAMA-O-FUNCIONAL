module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Digite o nome do arquivo: "
    arquivo <- getLine
    putStr "Digite a lista de inteiros: "
    lista <- getLine
    let conteudo = unlines (words lista)
    writeFile arquivo conteudo
    novoConteudo <- readFile arquivo
    let numeros = map read (words novoConteudo) :: [Int]
    mapM_ (putStrLn . show) numeros
    

    
