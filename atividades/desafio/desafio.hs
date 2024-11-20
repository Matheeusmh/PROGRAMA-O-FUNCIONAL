module Main (main) where

import System.IO

media :: (Double)->(Double)->Double
media nota1 nota2 = (nota1 + nota2) / 2

situacao :: (Double) -> String
situacao media 
 | media >= 5 && media <= 6 = "exame especial"
 | media > 6 = "aprovado"
 | otherwise = "reprovado"

main :: IO ()
main = do
    conteudo <- readFile "a.txt"
    writeFile "dados.txt" (processando conteudo)
    where
        processando conteudo = unlines (map processandoLinha (lines conteudo))
        processandoLinha linha = 
            let [codigo, nome, nota1, nota2] = words linha
                n1 = read nota1 :: Double
                n2 = read nota2 :: Double
                m = media n1 n2
                s = situacao m
            in unwords [codigo, nome, nota1, nota2, show m, s]