
module Graph where
import AST (Pos, Type, Unop, Binop)
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.PatriciaTree as PT
import qualified Data.GraphViz as GV
import qualified Data.GraphViz.Commands as GVC

type NodeID = Integer
data NodeLabel = NodeLabel
    { node_id    :: NodeID
    , node_pos   :: Pos
    , node_label :: Maybe String -- If their was an explicit label
                                 -- in the source code
    , node_out   :: [EdgeID]
    , node_in    :: [EdgeID]
    } deriving (Show)

type EdgeID = Integer
data EdgeLabel = EdgeLabel
    { edge_id   :: EdgeID
    , edge_src  :: NodeID
    , edge_dst  :: NodeID
    , edge_inst :: EdgeInst
    } deriving (Show)

-- We accepts variables of type void, but their evaluation result
-- in a bottom expression
type EVarID = Integer
data EdgeVar = EdgeVar
    { edge_var_id   :: EVarID
    , edge_var_name :: String
    , edge_var_type :: Type ()
    , edge_var_pos  :: Pos
    } deriving (Show)

data EdgeExpr =
      EEconst Integer
    | EEvar   EVarID
    | EInter  EdgeExpr EdgeExpr -- Support for random integers in interval
    | EEbinop (Binop Pos) EdgeExpr EdgeExpr
    | EEunop  (Unop  Pos) EdgeExpr
    deriving (Show)

type FunID = Integer
data Function = Function
    { function_id       :: FunID
    , function_name     :: String
    , function_entry    :: NodeID
    , function_exit     :: NodeID
    , function_params   :: [EVarID]
    , function_ret      :: EVarID -- If function doesn't return anything,
                                  -- function_ret must be of type void
    , function_ret_type :: Type ()
    } deriving (Show)

data EdgeInst =
      EIassign EVarID EdgeExpr
    | EInop    -- May be necessary sometimes
    | EIcall   FunID
    | EIguard  EdgeExpr
    | EIassert EdgeExpr
    deriving (Show)

data Program = Program
    { program_vars       :: [EdgeVar]
    , program_top_vars   :: [EVarID] -- Included in program vars
    , program_nodes      :: [NodeLabel]
    , program_edges      :: [EdgeLabel]
    , program_init_entry :: NodeID
    , program_init_exit  :: NodeID
    , program_functions  :: [Function]
    } deriving (Show)

getNodeByID :: Program -> NodeID -> NodeLabel
getNodeByID prg nid = head $ filter ((== nid) . node_id) $ program_nodes prg

getEdgeByID :: Program -> EdgeID -> EdgeLabel
getEdgeByID prg eid = head $ filter ((== eid) . edge_id) $ program_edges prg

getFunByName :: Program -> String -> Function
getFunByName prg name = head $ filter ((== name) . function_name) $ program_functions prg

getFunByID :: Program -> FunID -> Function
getFunByID prg fid = head $ filter ((== fid) . function_id) $ program_functions prg

makeFGL :: Graph gr => Program -> gr String String
makeFGL prg = mkGraph nodes edges
 where nodes = zip  (map (fromInteger . node_id) $ program_nodes prg)
                    (map (show . node_pos) $ program_nodes prg)
       edges = zip3 (map (fromInteger . edge_src) $ program_edges prg)
                    (map (fromInteger . edge_dst) $ program_edges prg)
                    (map (show . edge_inst) $ program_edges prg)

-- Write a program to the standart output in dot format
writeDot :: Program -> IO ()
writeDot prg = GVC.runGraphviz dot GV.Pdf "graph.pdf" >> return ()
 where fgl = makeFGL prg :: PT.Gr String String
       dot = GV.graphToDot GV.nonClusteredParams fgl

