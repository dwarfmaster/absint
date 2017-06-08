
module Builder (build_graph) where
import AST
import Graph
import Control.Monad (foldM)
import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as M

build_graph :: File -> Program
build_graph file = fst $ runState (buildFile file) initGraphState

type Binding = Map String EVarID
data GraphState = GraphState
    { st_lastID   :: Integer
    , st_bindings :: [Binding]
    , st_returns  :: [(NodeID, Maybe EdgeExpr)]
    , st_labels   :: Map String NodeID
    , st_gotos    :: [(String,NodeID)]
    , st_program  :: Program
    }

type St = State GraphState

initGraphState :: GraphState
initGraphState = GraphState 0 [M.empty] [] M.empty [] $
    Program [] [] [] [] 0 0 []

addReturn :: NodeID -> Maybe EdgeExpr -> St ()
addReturn nid me = do
    s <- get
    put $ s { st_returns = (nid,me) : (st_returns s) }

addGoto :: String -> NodeID -> St ()
addGoto label from = do
    s <- get
    put $ s { st_gotos = (label,from) : (st_gotos s) }

updateProgram :: (Program -> Program) -> St ()
updateProgram f = do
    s <- get
    put $ s { st_program = f (st_program s) }

updateHead :: (a -> a) -> [a] -> [a]
updateHead f [] = []
updateHead f (h : t) = f h : t

updateBinding :: (Binding -> Binding) -> St ()
updateBinding f = do
    s <- get
    put $ s { st_bindings = updateHead f (st_bindings s) }
 
updateLabels :: (Map String NodeID -> Map String NodeID) -> St ()
updateLabels f = do
    s <- get
    put $ s { st_labels = f (st_labels s) }

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
addVar vid name tp pos prg = prg { program_vars = var : filter ((/= vid) . edge_var_id) (program_vars prg) }
 where var = EdgeVar vid name tp pos

addTopVar :: EVarID -> String -> Type () -> Pos -> Program -> Program
addTopVar vid name tp pos prg = prg { program_top_vars = var : (program_top_vars prg) }
 where var = EdgeVar vid name tp pos

newVarID :: St EVarID
newVarID = newID

newVar :: String -> Type () -> Pos -> St EVarID
newVar name tp pos = do
    vid <- newVarID
    updateProgram $ addVar vid name tp pos
    updateBinding $ M.insert name vid
    return vid

hasVar :: String -> St Bool
hasVar name = do
    s <- get
    return $ M.member name $ head $ st_bindings s

newTopVar :: String -> Type () -> Pos -> St EVarID
newTopVar name tp pos = do
    vid <- newVarID
    updateProgram $ addTopVar vid name tp pos
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
    return $ (head $ st_bindings s) ! name

pushBindings :: St ()
pushBindings = do
    s <- get
    let bnds = st_bindings s
    put $ s { st_bindings = (head bnds) : bnds }

popBindings :: St ()
popBindings = do
    s <- get
    put $ s { st_bindings = tail $ st_bindings s }


-- The node_out and node_in are filled in a second processing of the built graph
addNode :: NodeID -> Pos -> Maybe String -> Program -> Program
addNode nid pos str prg = prg { program_nodes = node : (program_nodes prg) }
 where node = NodeLabel nid pos str [] []

newNodeID :: St NodeID
newNodeID = newID

newNode :: Pos -> Maybe String -> St NodeID
newNode pos Nothing = do
    nid <- newNodeID
    updateProgram $ addNode nid pos Nothing
    return nid
newNode pos (Just label) = do
    nid <- newNodeID
    updateProgram $ addNode nid pos $ Just label
    updateLabels  $ M.insert label nid
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

hasFunction :: String -> St Bool
hasFunction name = get >>=
    \s -> return $ not $ null
                 $ filter ((== name) . function_name)
                 $ program_functions $ st_program s

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

buildExpr nid (Evar (Ident name, _)) = getVar name >>= \v -> return (nid, EEvar v)

--      dps      f()       e
-- (nr) ->- (n1) ->- (n2) ->- (nid)
buildExpr nid (Ecall (Ident f, pos) lst) = do
    n1        <- newNode pos Nothing
    n2        <- newNode pos Nothing
    (nr,prms) <- foldM fld (n1,[]) $ reverse lst
    ret       <- make_call pos n1 n2 f prms
    tp        <- getFunType f
    nv        <- newVar' tp pos
    _         <- newEdge n2 nid (EIassign nv (EEvar ret))
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

make_call :: Pos -> NodeID -> NodeID -> String -> [EdgeExpr] -> St EVarID
make_call _ n1 n2 name [] = do
    fid <- getFun function_id name
    _   <- newEdge n1 n2 (EIcall fid)
    getFun function_ret name

make_call pos n1 n2 name params_values = do
    params_names <- getFun function_params name
    let params = zip params_names params_values
    n  <- newNode pos Nothing
    n' <- foldM fld n $ reverse $ tail params
    let (v,e) = head params
    _  <- newEdge n1 n' (EIassign v e)
    make_call pos n n2 name []
 where fld n (v,e) = do
           n' <- newNode pos Nothing
           _  <- newEdge n' n (EIassign v e)
           return n'



-- The last NodeID is the out of the closest loop (for break)
buildInstr' :: NodeID -> NodeID -> NodeID -> Instr' Pos -> St ()
buildInstr' n1 n2 lout (Iblock []) = buildInstr' n1 n2 lout Inop
buildInstr' n1 n2 lout (Iblock lst) = do
    pushBindings
    forM_ lst handleDecl'
    n <- foldM (\n (i,_) -> buildInstr n lout i) n2 $ reverse lst
    _ <- newEdge n1 n EInop
    popBindings
    return ()
 where handleDecl' :: Ann Instr Pos -> St ()
       handleDecl' (Instr _ (i,_), _) = handleDecl i
       handleDecl :: Instr' Pos -> St ()
       handleDecl (Idecl (tp, _) lst) =
           forM_ lst $ \((Ident name, pos), _) -> newVar name (fmap (const ()) tp) pos >> return ()
       handleDecl _ = return ()

buildInstr' n1 n2 lout Inop = 
    if n1 == n2 then return ()
    else do
        _ <- newEdge n1 n2 (EInop)
        return ()

buildInstr' n1 n2 lout Ibreak = newEdge n1 lout EInop >>= const (return ())

-- Assume lst is never empty here
buildInstr' n1 n2 lout (Idecl (tp, _) lst) = do
    n3 <- foldM handleDecl n2 $ reverse lst
    _  <- newEdge n1 n3 EInop
    return ()
 where handleDecl n2 ((Ident name, pos), Nothing) = do
           nv <- getVar name
           n1 <- newNode pos Nothing
           _  <- newEdge n1 n2 (EIassign nv (EEconst 0))
           return n1
       handleDecl n2 ((Ident name, pos), Just (e, epos)) = do
           nv        <- getVar name
           n2'       <- newNode pos Nothing
           (n1', ne) <- buildExpr n2' e
           _         <- newEdge n2' n2 (EIassign nv ne)
           return n1'

-- Assume lst is never empty here
buildInstr' n1 n2 lout (Iassign lst) = do
    n3 <- foldM handleAssign n2 $ reverse lst
    _  <- newEdge n1 n3 EInop
    return ()
 where handleAssign n2 ((Ident name, pos), (e, epos)) = do
           nn        <- newNode pos Nothing
           v         <- getVar name
           (nn', ne) <- buildExpr nn e
           _         <- newEdge nn n2 (EIassign v ne)
           return nn'

buildInstr' n1 n2 lout (Icall f lst) = do
    (n3,_) <- buildExpr n2 (Ecall f lst)
    _      <- newEdge n1 n3 EInop
    return ()

--                Ge       ii
--               /->- (n4) ->-\
--       e      /              \
-- (n1) ->- (n3)               (n2)
--              \ G!e      ie  /
--               \->- (n5) ->-/
buildInstr' n1 n2 lout (Iif (e, pos) (ii, pii) (ie, pie)) = do
    n3       <- newNode pos Nothing
    (n1',ne) <- buildExpr n3 e
    _        <- newEdge n1 n1' EInop
    n4       <- buildInstr n2 lout ii 
    _        <- newEdge n3 n4 (EIguard ne)
    n5       <- buildInstr n2 lout ie
    _        <- newEdge n3 n5 (EIguard $ EEunop Unot ne)
    return ()

--                      G!e
--                     /->- (n2)
--      nop        e   |   Ge        i
-- (n1) ->- (n1') ->- (n3) ->- (n4) ->- (n5)
--            |            nop           |
--            \-------------<------------/
buildInstr' n1 n2 lout (Iwhile (e,pe) (i,pi)) = do
    n5       <- newNode pi Nothing
    n4       <- buildInstr n5 lout i
    n3       <- newNode pe Nothing
    (n1',ne) <- buildExpr n3 e
    _        <- newEdge n3 n4 (EIguard ne)
    _        <- newEdge n3 n2 (EIguard $ EEunop Unot ne)
    _        <- newEdge n1 n1' EInop
    return ()

--                      G!e
--                     /->- (n2)
--      i1        e    |  Ge       bd       i2
-- (n1) ->- (n3) ->- (n4) ->- (n5) ->- (n6) ->- (n7)
--            |                 nop               |
--            \------------------<----------------/
buildInstr' n1 n2 lout (Ifor (i1, pi1) (e, pe) (i2, pi2) (bd, pbd)) = do
    n7      <- newNode pi2 Nothing
    n6      <- buildInstr n7 n2 i2
    n5      <- buildInstr n6 n2 bd
    n4      <- newNode pe  Nothing
    (n3,ne) <- buildExpr n4 e
    n1'     <- buildInstr n3 lout i1
    _       <- newEdge n1 n1' EInop
    _       <- newEdge n4 n5 (EIguard ne)
    _       <- newEdge n4 n2 (EIguard $ EEunop Unot ne)
    _       <- newEdge n7 n3 EInop
    return ()

buildInstr' n1 n2 lout (Igoto (Ident label, pos)) = addGoto label n1

buildInstr' n1 n2 lout (Ireturn Nothing) = addReturn n1 Nothing
buildInstr' n1 n2 lout (Ireturn (Just (e,pos))) = do
    n3       <- newNode pos Nothing
    (n1',ne) <- buildExpr n3 e
    _        <- newEdge n1 n1' EInop
    addReturn n3 $ Just ne

buildInstr :: NodeID -> NodeID -> Instr Pos -> St NodeID
buildInstr n lout (Instr Nothing (i,pos)) = do
    n1 <- newNode pos Nothing
    buildInstr' n1 n lout i
    return n1
buildInstr n lout (Instr (Just (Ident l,_)) (i,pos)) = do
    n1 <- newNode pos (Just l)
    buildInstr' n1 n lout i
    return n1




buildTopLevel :: NodeID -> TopLevel Pos -> St NodeID
buildTopLevel n2 (TdeclVar (tp, _) lst) = do
    n1 <- foldM handleDecl n2 $ reverse lst
    return n1
 where handleDecl n2 ((Ident name, pos), Nothing) = do
           nv <- newTopVar name (fmap (const ()) tp) pos
           n1 <- newNode pos Nothing
           _  <- newEdge n1 n2 (EIassign nv (EEconst 0))
           return n1
       handleDecl n2 ((Ident name, pos), Just (e, epos)) = do
           nv        <- newTopVar name (fmap (const ()) tp) pos
           n2'       <- newNode pos Nothing
           (n1', ne) <- buildExpr n2' e
           _         <- newEdge n2' n2 (EIassign nv ne)
           return n1'

buildTopLevel n2 (TdeclFun (tp, _) (Ident name, pos) prms) = do
    b <- hasFunction name
    if b then return n2
    else do
        fbeg <- newNode pos Nothing
        fend <- newNode pos Nothing
        args <- forM prms $ \((Ident v, pos), (tp, _)) -> newVar v (fmap (const ()) tp) pos
        ret  <- newVar' (fmap (const ()) tp) pos
        _    <- newFun name fbeg fend args ret (fmap (const ()) tp)
        return n2

buildTopLevel n2 (TimplFun tp nm@(Ident name,_) prms (i, _)) = do
    pushBindings
    _  <- buildTopLevel n2 (TdeclFun tp nm prms)
    nb <- getFun function_entry name
    ne <- getFun function_exit  name
    n  <- buildInstr nb nb i
    popBindings
    _  <- newEdge nb n EInop
    return n2

buildFile :: File -> St Program
buildFile [] = return $ Program [] [] [] [] 0 0 []
buildFile l@((_,pos) : _) = do
    n2 <- newNode pos Nothing
    n1 <- foldM buildTopLevel n2 $ map fst l
    setInitEntry n1
    setInitExit  n2
    s <- get
    return $ st_program s


