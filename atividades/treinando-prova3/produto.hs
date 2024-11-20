module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

prompt mensagem = do {putStr mensagem; readLn :: IO Float}
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    n1 <- prompt "Digite um numero: "
    n2 <- prompt "Digite um numero: "
    n3 <- prompt "Digite outro numero: "

    putStr ("Resultado: " ++ show (n1 * n2 * n3))