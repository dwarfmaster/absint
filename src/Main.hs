
module Main where
import Parser
import Builder

main :: IO ()
main = do
    s <- getContents
    case parseFile s "<stdin>" of
     Left e   -> putStrLn $ "Error : " ++ e
     Right fl -> putStrLn $ show $ build_graph fl

