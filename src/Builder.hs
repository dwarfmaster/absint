
module Builder (build_graph) where
import AST
import Graph
import Control.Monad (foldM)
import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as M

build_graph :: File -> Program
build_graph = undefined

type Binding = Map String EVarID
data GraphState = GraphState
    { st_lastID   :: Integer
    , st_bindings :: Binding
    , st_program  :: Program
    }

type St = State GraphState

initGraphState :: GraphState
initGraphState = GraphState 0 M.empty $
    Program [] [] [] 0 0 []

updateProgram :: (Program -> Program) -> St ()
updateProgram f = do
    s <- get
    put $ s { st_program = f (st_program s) }

updateBinding :: (Binding -> Binding) -> St ()
updateBinding f = do
    s <- get
    put $ s { st_bindings = f (st_bindings s) }

newID :: St Integer
newID = do
    s <- get
    let nid = st_lastID s
    put $ s { st_lastID = nid + 1 }
    return nid

setInitEntry :: NodeID -> St ()
setInitEntry nid = updateProgram $ \p -> p { program_init_entry = nid }

setInitExit :: NodeID -> St ()
setInitExit nid = updateProgram $ \p -> p { program_init_exit = nid }



addVar :: EVarID -> String -> Type () -> Pos -> Program -> Program
addVar vid name tp pos prg = prg { program_vars = var : (program_vars prg) }
 where var = EdgeVar vid name tp pos

newVarID :: St EVarID
newVarID = newID

newVar :: String -> Type () -> Pos -> St EVarID
newVar name tp pos = do
    vid <- newVarID
    updateProgram $ addVar vid name tp pos
    updateBinding $ M.insert name vid
    return vid

newVar' :: Type () -> Pos -> St EVarID
newVar' tp pos = do
    vid <- newVarID
    updateProgram $ addVar vid ("__" ++ show vid) tp pos
    return vid

getVar :: String -> St EVarID
getVar name = do
    s <- get
    return (st_bindings s ! name)


-- The node_out and node_in are filled in a second processing of the built graph
addNode :: NodeID -> Pos -> Maybe String -> Program -> Program
addNode nid pos str prg = prg { program_nodes = node : (program_nodes prg) }
 where node = NodeLabel nid pos str [] []

newNodeID :: St NodeID
newNodeID = newID

newNode :: Pos -> Maybe String -> St NodeID
newNode pos str = do
    nid <- newNodeID
    updateProgram $ addNode nid pos str
    return nid



addEdge :: EdgeID -> NodeID -> NodeID -> EdgeInst -> Program -> Program
addEdge eid src dst inst prg = prg { program_edges = edge : (program_edges prg) }
 where edge = EdgeLabel eid src dst inst

newEdgeID :: St EdgeID
newEdgeID = newID

newEdge :: NodeID -> NodeID -> EdgeInst -> St EdgeID
newEdge src dst inst = do
    eid <- newEdgeID
    updateProgram $ addEdge eid src dst inst
    return eid



addFun :: FunID -> String -> NodeID -> NodeID -> [EVarID] -> EVarID -> Type () -> Program -> Program
addFun fid name entry exit prms ret tp prg = prg { program_functions = fn : (program_functions prg) }
 where fn = Function fid name entry exit prms ret tp

newFunID :: St FunID
newFunID = newID

newFun :: String -> NodeID -> NodeID -> [EVarID] -> EVarID -> Type () -> St FunID
newFun name entry exit prms ret tp = do
    fid <- newFunID
    updateProgram $ addFun fid name entry exit prms ret tp
    return fid

getFun :: (Function -> a) -> String -> St a
getFun f name = get >>=
    \s -> return $ f
          $ head $ filter ((== name) . function_name)
          $ program_functions $ st_program s

getFunType :: String -> St (Type ())
getFunType = getFun function_ret_type



-- TODO make it check all orders of computation
buildExpr :: NodeID -> Expr Pos -> St (NodeID, EdgeExpr)
buildExpr nid (Econst x) = return (nid, EEconst x)

buildExpr nid (Evar (Ident name, pos)) = getVar name >>= \v -> return (nid, EEvar v)

--      dps      f()       e
-- (nr) ->- (n1) ->- (n2) ->- (nid)
buildExpr nid (Ecall (Ident f, pos) lst) = do
    n1        <- newNode pos Nothing
    n2        <- newNode pos Nothing
    (nr,prms) <- foldM fld (n1,[]) $ reverse lst
    ret       <- make_call' pos n1 n2 f prms
    tp        <- getFunType f
    nv        <- newVar' tp pos
    newEdge n2 nid (EIassign nv (EEvar ret))
    return (nr, EEvar nv)
 where fld (nid',l) x = do
            (nid'', expr) <- buildExpr nid' $ fst x
            return (nid'', expr : l)

--      e1       e2
-- (nr) ->- (n1) ->- (nid)
buildExpr nid (Ebinop e1 (op, _) e2) = do
    (n1, e2') <- buildExpr nid $ fst e2
    (nr, e1') <- buildExpr n1  $ fst e1
    return (nr, EEbinop op e1' e2')

buildExpr nid (Eunop (uop, _) e) = do
    (nr, e') <- buildExpr nid $ fst e
    return (nr, EEunop uop e')

make_call :: Pos -> NodeID -> String -> [EdgeExpr] -> St (NodeID, EVarID)
make_call pos n2 name params = do
    n1 <- newNode pos Nothing
    ev <- make_call' pos n1 n2 name params
    return (n1,ev)

make_call' :: Pos -> NodeID -> NodeID -> String -> [EdgeExpr] -> St EVarID
make_call' pos n1 n2 name [] = do
    fid <- getFun function_id name
    newEdge n1 n2 (EIcall fid)
    getFun function_ret name

make_call' pos n1 n2 name params_values = do
    params_names <- getFun function_params name
    let params = zip params_names params_values
    n  <- newNode pos Nothing
    n' <- foldM fld n $ tail $ reverse params
    let (v,e) = head params
    newEdge n1 n' (EIassign v e)
    make_call' pos n n2 name []
 where fld n (v,e) = do
           n' <- newNode pos Nothing
           newEdge n' n (EIassign v e)
           return n'

