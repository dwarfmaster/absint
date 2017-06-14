
module Main where
import Parser
import Builder
import Graph
import Concrete
import Constant
import Segment
import Domain
import qualified Iterator as It
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

showResult :: Show a => Map NodeID a -> String
showResult mp = M.foldWithKey (\k v s -> showa k v ++ s) "" mp
 where showa i a = "\n\n" ++ emph ++ show i ++ emph ++ "\n" ++ show a
       emph = take 30 $ repeat '*'

varBindings :: Program -> String
varBindings prg = L.intercalate "\n" $ map (\v -> show (edge_var_id v) ++ " -> " ++ mshow v) $ program_vars prg
 where mshow var = edge_var_name var ++ "-" ++ (show $ edge_var_pos var)

writeAnalysis :: Show a => FilePath -> Program -> Map NodeID a -> IO ()
writeAnalysis file prg res = writeFile file $ varBindings prg ++ showResult res

main :: IO ()
main = do
    s <- getContents
    case parseFile s "<stdin>" of
     Left e   -> putStrLn $ "Error : " ++ e
     Right fl -> do let prg = build_graph fl
                    writeDot prg
                    writeAnalysis "out.concrete" prg (It.iterate prg :: Map NodeID (DomainAbstract IntSet))
                    writeAnalysis "out.constant" prg (It.iterate prg :: Map NodeID (DomainAbstract ConstantDomain))
                    writeAnalysis "out.segment"  prg (It.iterate prg :: Map NodeID (DomainAbstract Segment))

