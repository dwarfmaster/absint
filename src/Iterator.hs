
module Iterator where
import Graph
import Data.Map (Map)

import Control.Monad (forM_, when)
import Control.Monad.ST
import qualified Data.STRef as R
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.Hashable as HSH
import qualified Data.Sequence as S
type HashTable s k v = HT.HashTable s k v

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

iterate :: Abstract a => Program -> Map NodeID a
iterate prg = undefined

queryOrInit :: (Eq k, HSH.Hashable k) => v -> k -> HashTable s k v -> ST s v
queryOrInit def key hash = do
    lkt <- HT.lookup hash key
    case lkt of
     Nothing -> HT.insert hash key def >> return def
     Just vl -> return vl

type Return = (NodeID, [EVarID])

while :: Monad m => m Bool -> m () -> m ()
while condition action = condition >>= \b -> if b then action >> while condition action else return ()

worklist :: Abstract a => Program -> ST s (HashTable s NodeID a)
worklist program = do
    -- Global variables
    queue    <- R.newSTRef S.empty
    asserts  <- R.newSTRef []
    inQueue  <- HT.new
    nbSeens  <- HT.new
    abstract <- HT.new
    returns  <- HT.new
    toclear  <- HT.new

    -- Initialisation
    enqueue queue inQueue $ program_init_entry program
    let main_entry = function_entry $ getFunByName program "main"
    addReturn returns (program_init_exit program) main_entry $ program_top_vars program

    while (emptyNQueue queue) $ do
        Just eid <- dequeue queue inQueue
        let edge = getEdgeByID program eid
        let src  = edge_src edge
        let dst  = edge_dst edge

        -- Widening
        incrSeen nbSeens eid
        count <- queryOrInit 0 eid nbSeens
        if count == 3 then updateNode program queue inQueue abstract toclear src widen
        else return ()

        -- Abstract instruction
        let update = updateNode program queue inQueue abstract toclear dst
        absdst <- queryOrInit bottom dst abstract
        case edge_inst edge of
         EIassign vid expr -> update $ assign vid expr
         EInop             -> update $ id
         EIguard expr      -> update $ guard expr

         EIcall fun        -> do let function = getFunByID program fun
                                 let beg = function_entry function
                                 let end = function_exit  function
                                 let ret = function_ret   function
                                 -- Call act as a INop to the beginning node
                                 updateNode program queue inQueue abstract toclear beg id
                                 HT.insert toclear beg True
                                 -- Setup return
                                 addReturn returns end dst $ ret : program_top_vars program

         EIassert expr     -> do update $ guard expr
                                 absdst' <- queryOrInit bottom dst abstract
                                 if isEq absdst absdst' then return ()
                                 else R.readSTRef asserts >>= \l -> R.writeSTRef asserts (expr : l)

        -- Handle returns
        mret <- popReturn returns dst
        case mret of
         Nothing          -> return ()
         Just (node,vars) -> queryOrInit bottom dst abstract >>=
                             \a -> updateNode program queue inQueue abstract toclear node $ backport vars a

    return abstract
 where dequeue :: R.STRef s (S.Seq EdgeID) -> HT.HashTable s EdgeID Bool -> ST s (Maybe EdgeID)
       dequeue queue inQueue = do
           q <- R.readSTRef queue
           case S.viewr q of
            S.EmptyR  -> return Nothing
            nq S.:> h -> do
                R.writeSTRef queue nq
                HT.insert inQueue h False
                return $ Just h

       enqueue :: R.STRef s (S.Seq EdgeID) -> HT.HashTable s EdgeID Bool -> EdgeID -> ST s ()
       enqueue queue inQueue e = do
           b <- queryOrInit False e inQueue
           if b then return ()
                else R.readSTRef queue >>= \q -> R.writeSTRef queue $ e S.<| q

       emptyNQueue :: R.STRef s (S.Seq EdgeID) -> ST s Bool
       emptyNQueue queue = R.readSTRef queue >>= \q -> return $ not $ S.null q

       incrSeen :: HT.HashTable s EdgeID Integer -> EdgeID -> ST s ()
       incrSeen nbSeens edge = do
           v <- queryOrInit 0 edge nbSeens
           HT.insert nbSeens edge $ v + 1

       addReturn :: HT.HashTable s NodeID [Return] -> NodeID -> NodeID -> [EVarID] -> ST s ()
       addReturn hash key v1 v2 = do
           val <- queryOrInit [] key hash
           HT.insert hash key $ (v1, v2) : val

       popReturn :: HT.HashTable s NodeID [Return] -> NodeID -> ST s (Maybe Return)
       popReturn hash key = do
           lkt <- HT.lookup hash key
           case lkt of
               Nothing      -> return Nothing
               Just []      -> return Nothing
               Just (h : t) -> HT.insert hash key t >> return (Just h)
       
       isEq :: Abstract a => a -> a -> Bool
       isEq a1 a2 = subset a1 a2 && subset a2 a1

       updateNode :: Abstract a => Program -> R.STRef s (S.Seq EdgeID)
                  -> HT.HashTable s EdgeID Bool -> HT.HashTable s NodeID a
                  -> HT.HashTable s NodeID Bool
                  -> NodeID -> (a -> a) -> ST s ()
       updateNode program queue inQueue abstract toClear n f = do
           must_clear <- queryOrInit False n toClear
           a <- if must_clear then return bottom
                              else queryOrInit bottom n abstract
           let a' = join a $ f a
           if isEq a a' then return ()
           else do
               HT.insert abstract n $ a'
               let eds = node_out $ getNodeByID program n
               forM_ eds $ \eid -> do enqueue queue inQueue eid
                                      let edge = getEdgeByID program eid
                                      when must_clear $ HT.insert toClear (edge_dst edge) True
               return ()
           HT.insert toClear n False


