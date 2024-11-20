module Main (main) where 

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Digite uma frase: "
    frase <- getLine

    let fraseInvertida = reverse (removeEspaco frase)
    let fraseOf = (removeEspaco frase)

    if fraseOf == fraseInvertida 
        then do
         putStr "\nA frase eh um palindromo!\n"
        else do 
         putStr "\nA frase NAO eh um palindromo!\n"

removeEspaco :: String -> String
removeEspaco palavra = filter (/= ' ') palavra