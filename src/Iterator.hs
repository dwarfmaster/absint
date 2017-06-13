
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
iterate program = M.fromList $ runST $ iterator >>= tolist
 where tolist :: HT.HashTable s NodeID a -> ST s [(NodeID, a)]
       tolist = HT.foldM (\l e -> return $ e : l) []
       iterator :: (Show a, Abstract a) => ST s (HT.HashTable s NodeID a)
       iterator = do
           abstract <- HT.new
           worklist program (program_init_entry program) abstract
           let main_entry = function_entry $ getFunByName program "main"
           abst <- queryOrInit bottom (program_init_exit program) abstract
           HT.insert abstract main_entry abst
           worklist program main_entry abstract
           return abstract

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
type AbsTable s a = HT.HashTable s NodeID a
worklist :: (Show a, Abstract a) => Program -> NodeID
         -> HT.HashTable s NodeID a -> ST s () -- Hashtable is changed by side-effect
worklist program firstNode abstract = do
    -- Global variables
    queue    <- R.newSTRef S.empty -- :: Queue s
    asserts  <- R.newSTRef []
    inQueue  <- HT.new             -- :: Mark s
    nbSeens  <- HT.new             -- :: Count s
    first    <- HT.new             -- :: Mark s

    -- Initialisation
    enqueue queue inQueue firstNode

    let update = updateNode program queue inQueue first abstract
    let absEdge = abstractEdge program queue inQueue abstract asserts
    while (emptyNQueue queue) $ do
        Just dst <- dequeue queue inQueue
        traceM $ "Handling " ++ show dst

        -- Widening
        incrSeen nbSeens dst
        count <- queryOrInit 0 dst nbSeens
        let can_widen = node_widen $ getNodeByID program dst
        let final = if (can_widen && count == 3) || count >= 10 then widen else id

        -- Abstract instruction
        update dst final $ absEdge dst

    return ()
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

       isEq :: Abstract a => a -> a -> Bool
       isEq a1 a2 = subset a1 a2 && subset a2 a1

       readEdge :: Abstract a => Program -> AbsTable s a
                -> EdgeID -> ST s (a, EdgeInst)
       readEdge program abstract eid = do
           let edge = getEdgeByID program eid
           let src  = edge_src edge
           abst <- queryOrInit bottom src abstract
           return (abst, edge_inst edge)

       abstractEdge :: (Show a, Abstract a) => Program -> Queue s -> Mark s
                    -> AbsTable s a -> R.STRef s [EdgeExpr]
                    -> NodeID -> EdgeID -> (a, EdgeInst) -> ST s a
       abstractEdge program queue inQueue abstract asserts dst eid (abst, inst) =
           case inst of
            EIassign vid expr -> return $ assign vid expr abst
            EInop             -> return $ abst
            EIguard expr      -> return $ guard expr abst

            -- This method could handle recursive function, but won't terminate.
            -- Thus, to handle recursive functions, all that is needed is to ensure
            -- terminaison of analysis of those (for example by not making the recursive
            -- call in bottom environmnent, more another check).
            EIcall fun        -> do
                let function = getFunByID program fun
                let beg      = function_entry function
                let end      = function_exit  function
                let ret      = function_ret   function
                -- Copy the abstract
                abstract' <- HT.new
                HT.mapM_ (\(k,v) -> HT.insert abstract' k v) abstract
                HT.insert abstract' beg abst
                worklist program beg abstract'
                abst' <- queryOrInit bottom end abstract'
                return $ backport (ret : program_top_vars program) abst' abst

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


