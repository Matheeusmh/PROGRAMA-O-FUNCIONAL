module Main (main) where

main :: IO ()
main = do 
    putStrLn "Digite o conteudo: "
    lerTeclado

lerTeclado ::  IO ()
lerTeclado = do
    c <- getLine
    pegarConteudo c

pegarConteudo :: String -> IO ()
pegarConteudo c = 
    case c of
        "0" -> lerArquivo
        _ -> appendFile "arq.txt" c >> lerTeclado

lerArquivo :: IO ()
lerArquivo = do
    conteudo <- readFile "arq.txt"
    putStrLn conteudo
    writeFile "arq.txt" ""