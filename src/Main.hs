
module Main where
import Parser
import AST

main :: IO ()
main = do
    s <- getContents
    case parseFile s "<stdin>" of
     Left e   -> putStrLn $ "Error : " ++ e
     Right fl -> putStrLn $ showFile fl

