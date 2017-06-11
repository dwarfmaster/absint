
module Iterator where
import Graph
import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad (forM_, forM)
import Control.Monad.ST
import qualified Data.STRef as R
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.Hashable as HSH
import qualified Data.Sequence as S
import Debug.Trace

class Abstract a where
    bottom   :: a
    assign   :: EVarID -> EdgeExpr -> a -> a
    guard    :: EdgeExpr -> a -> a
    widen    :: a -> a
    subset   :: a -> a -> Bool
    join     :: a -> a -> a
    -- Take the values of the vars in list in the first abstraction
    -- and put them in the second one : necessary to handle recursion
    backport :: [EVarID] -> a -> a -> a

iterate :: (Show a, Abstract a) => Program -> Map NodeID a
iterate program = M.fromList $ runST $ worklist program >>= tolist
 where tolist :: HT.HashTable s NodeID a -> ST s [(NodeID, a)]
       tolist = HT.foldM (\l e -> return $ e : l) []

queryOrInit :: (Eq k, HSH.Hashable k) => v -> k -> HT.HashTable s k v -> ST s v
queryOrInit def key hash = do
    lkt <- HT.lookup hash key
    case lkt of
     Nothing -> HT.insert hash key def >> return def
     Just vl -> return vl

while :: Monad m => m Bool -> m () -> m ()
while condition action = condition >>= \b -> if b then action >> while condition action else return ()

type Queue s      = R.STRef s (S.Seq NodeID)
type Mark s       = HT.HashTable s NodeID Bool
type Count s      = HT.HashTable s NodeID Integer
type Return s     = HT.HashTable s NodeID (Maybe (NodeID, Maybe EdgeID))
type AtReturn s a = HT.HashTable s EdgeID a
type AbsTable s a = HT.HashTable s NodeID a
worklist :: (Show a, Abstract a) => Program -> ST s (HT.HashTable s NodeID a)
worklist program = do
    -- Global variables
    queue    <- R.newSTRef S.empty -- :: Queue s
    asserts  <- R.newSTRef []
    inQueue  <- HT.new             -- :: Mark s
    nbSeens  <- HT.new             -- :: Count s
    abstract <- HT.new             -- :: AbsTable s a
    returns  <- HT.new             -- :: Return s
    atReturn <- HT.new             -- :: AtReturn s a
    first    <- HT.new             -- :: Mark s

    -- Initialisation
    enqueue queue inQueue $ program_init_entry program
    let main_entry = function_entry $ getFunByName program "main"
    setReturn returns (program_init_exit program) main_entry Nothing

    let update = updateNode program queue inQueue first abstract
    let absEdge = abstractEdge program queue inQueue returns abstract asserts atReturn
    while (emptyNQueue queue) $ do
        Just dst <- dequeue queue inQueue
        traceM $ "Handling " ++ show dst

        -- Widening
        incrSeen nbSeens dst
        count <- queryOrInit 0 dst nbSeens
        let final = if count == 3 then widen else id

        -- Abstract instruction
        update dst final $ absEdge dst

        -- Handle returns
        mret <- popReturn returns dst
        case mret of
         Nothing   -> return ()
         -- Act like a virtual EInop
         Just (node,Nothing) -> do a <- queryOrInit bottom dst abstract
                                   HT.insert abstract node a
                                   enqueue queue inQueue node
         Just (node,Just edge) -> do
             dst_abs <- queryOrInit bottom dst  abstract
             old_abs <- queryOrInit bottom edge atReturn
             if isEq dst_abs old_abs then return ()
                                     else do HT.insert atReturn edge dst_abs
                                             enqueue queue inQueue node

    return abstract
 where dequeue :: Queue s -> Mark s -> ST s (Maybe EdgeID)
       dequeue queue inQueue = do
           q <- R.readSTRef queue
           case S.viewr q of
            S.EmptyR  -> return Nothing
            nq S.:> h -> do
                R.writeSTRef queue nq
                HT.insert inQueue h False
                return $ Just h

       enqueue :: Queue s -> Mark s -> NodeID -> ST s ()
       enqueue queue inQueue n = do
           b <- queryOrInit False n inQueue
           if b then return ()
                else R.readSTRef queue >>= \q -> R.writeSTRef queue $ n S.<| q

       emptyNQueue :: Queue s -> ST s Bool
       emptyNQueue queue = R.readSTRef queue >>= \q -> return $ not $ S.null q

       incrSeen :: Count s -> NodeID -> ST s ()
       incrSeen nbSeens node = do
           v <- queryOrInit 0 node nbSeens
           HT.insert nbSeens node $ v + 1

       setReturn :: Return s -> NodeID -> NodeID -> Maybe EdgeID -> ST s ()
       setReturn hash key v1 v2 = HT.insert hash key $ Just (v1, v2)

       popReturn :: Return s -> NodeID -> ST s (Maybe (NodeID, Maybe EdgeID))
       popReturn hash key = queryOrInit Nothing key hash

       isEq :: Abstract a => a -> a -> Bool
       isEq a1 a2 = subset a1 a2 && subset a2 a1

       readEdge :: Abstract a => Program -> AbsTable s a
                -> EdgeID -> ST s (a, EdgeInst)
       readEdge program abstract eid = do
           let edge = getEdgeByID program eid
           let src  = edge_src edge
           abst <- queryOrInit bottom src abstract
           return (abst, edge_inst edge)

       abstractEdge :: Abstract a => Program -> Queue s -> Mark s -> Return s
                    -> AbsTable s a -> R.STRef s [EdgeExpr] -> AtReturn s a
                    -> NodeID -> EdgeID -> (a, EdgeInst) -> ST s a
       abstractEdge program queue inQueue returns abstract asserts retabs dst eid (abst, inst) =
           case inst of
            EIassign vid expr -> return $ assign vid expr abst
            EInop             -> return $ abst
            EIguard expr      -> return $ guard expr abst

            EIcall fun        -> do
                let function = getFunByID program fun
                let beg      = function_entry function
                let end      = function_exit  function
                let ret      = function_ret   function
                HT.insert abstract beg abst
                enqueue queue inQueue beg
                setReturn returns end dst $ Just eid
                reta <- queryOrInit bottom eid retabs
                return $ backport (ret : program_top_vars program) reta abst

            EIassert expr     -> do
                let a' = guard expr abst
                if isEq a' abst then return ()
                                else R.readSTRef asserts >>= \l -> R.writeSTRef asserts (expr : l)
                return a'

       updateNode :: (Show a, Abstract a) => Program -> Queue s -> Mark s -> Mark s -> HT.HashTable s NodeID a
                  -> NodeID -> (a -> a) -> (EdgeID -> (a, EdgeInst) -> ST s a) -> ST s ()
       updateNode program queue inQueue first abstract nid final f = do
           a <- queryOrInit bottom nid abstract
           let node = getNodeByID program nid
           let ins  = node_in node
           ains <- forM ins $ \eid -> readEdge program abstract eid >>= \r -> traceShowM r >> f eid r
           let a' = if null ins then a
                                else final $ foldr join bottom ains
           b <- queryOrInit True nid first
           HT.insert first nid False
           if isEq a a' && not b then return ()
           else do
               HT.insert abstract nid a'
               let eds = node_out $ getNodeByID program nid
               forM_ eds $ \eid -> enqueue queue inQueue $ edge_dst $ getEdgeByID program eid
               return ()


