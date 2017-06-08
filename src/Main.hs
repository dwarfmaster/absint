
module Main where
import Parser
import Builder
import Graph

main :: IO ()
main = do
    s <- getContents
    case parseFile s "<stdin>" of
     Left e   -> putStrLn $ "Error : " ++ e
     Right fl -> writeDot $ build_graph fl

