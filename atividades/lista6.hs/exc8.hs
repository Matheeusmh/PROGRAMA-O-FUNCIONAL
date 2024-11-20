module Main (main) where

import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    pegarInformacoes

pegarInformacoes :: IO()
pegarInformacoes = do
    putStr "Digite o nome do arquivo: "
    arquivo <- getLine
    conteudo <- readFile arquivo 
    subVogais conteudo

subVogais :: String -> IO ()
subVogais conteudo = do
    let novoConteudo = unlines (map (unwords . map sub . words) (lines conteudo))
    gravarConteudo novoConteudo
    where 
        sub [] = []
        sub (x:xs) | x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' = '*' : sub xs
                   | otherwise = x : sub xs
                  
gravarConteudo :: String -> IO ()
gravarConteudo conteudo = do
    writeFile "caseVogais.txt" conteudo
