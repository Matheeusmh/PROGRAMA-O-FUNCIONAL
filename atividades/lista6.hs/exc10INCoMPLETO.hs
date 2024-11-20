module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Data.Char(toLower)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Digite o nome do arquivo: "
    caminho <- getLine
    putStr "Digite a palavra que deseja contar: "
    palavra <- getLine

    conteudo <- readFile caminho

    let conteudoMinusculo = strToLower conteudo
    let palavraMinusculo = strToLower palavra

    let count = countPalavra conteudoMinusculo palavra in putStrLn (show count)

countPalavra :: String -> String -> Int
countPalavra conteudo palavra = map (\x . auxCount (words x)) (lines conteudo) 0
    where
        auxCount [] n = n
        auxCount (x:xs) n | palavra == x = auxCount xs (n + 1)
                          | otherwise = auxCount xs n

strToLower :: [Char] -> [Char]
strToLower [] = []
strToLower (x:xs) = (toLower x) : strToLower xs