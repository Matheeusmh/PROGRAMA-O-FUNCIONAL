module Main (main) where

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

prompt mensagem = do {putStr mensagem;readLn :: IO Int}

main :: IO ()
main = do 
    hSetBuffering stdout NoBuffering
    idade <- prompt "Digite a sua idade: "

    if idade < 16 
        then putStrLn "Nao eleitor!!"
        else if idade >= 16 && idade <= 18 || idade > 65 
            then putStrLn "Eleitor facultativo!"
            else putStrLn "Eleitor OBRIGATORIO!"