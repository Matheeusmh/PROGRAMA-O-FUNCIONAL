module Main (main) where

import System.IO(stdout, hSetBuffering, BufferMode(NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    pegarInformacoes ""

pegarInformacoes :: String -> IO ()
pegarInformacoes conteudo = do
    putStr "Digite o nome: "
    nome <- getLine
    putStr "Digite o numero: "
    numero <- getLine 
    let novoConteudo = conteudo ++ nome ++ "\n" in verificaNumero numero novoConteudo

verificaNumero :: String -> String -> IO ()
verificaNumero numero conteudo = do
    case numero of
        "0" -> gravarInformacoes conteudo
        _ -> let novoConteudo = conteudo ++ numero ++ "\n" in pegarInformacoes novoConteudo

gravarInformacoes :: String -> IO ()
gravarInformacoes conteudo = do
    writeFile "dadosTelefonicos.txt" conteudo