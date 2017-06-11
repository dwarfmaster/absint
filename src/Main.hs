
module Main where
import Parser
import Builder
import Graph
import Concrete
import Constant
import Domain
import qualified Iterator as It
import Data.Map (Map)

main :: IO ()
main = do
    s <- getContents
    case parseFile s "<stdin>" of
     Left e   -> putStrLn $ "Error : " ++ e
     Right fl -> do let prg = build_graph fl
                    writeDot prg
                    let analysis = It.iterate prg :: Map NodeID (DomainAbstract ConstantDomain)
                    putStrLn $ show analysis

