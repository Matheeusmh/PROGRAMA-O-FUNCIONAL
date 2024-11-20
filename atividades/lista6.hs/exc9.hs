module Main (main) where

import Data.Char (isLower, toUpper)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    lerDados

lerDados :: IO ()
lerDados = do
    putStr "Digite o nome do arquivo: "
    arquivo <- getLine
    conteudo <- readFile arquivo 
    converte conteudo

converte :: String -> IO()
converte conteudo = do
    let novoConteudo = unlines (map (unwords . map toUpperString . words) (lines conteudo)) in gravarConteudo novoConteudo
      where
        toUpperString [] = []
        toUpperString (x:xs) | isLower x = toUpper x : toUpperString xs
                             | otherwise = x : toUpperString xs

gravarConteudo :: String -> IO ()
gravarConteudo conteudo = do
    writeFile "allUpper.txt" conteudo