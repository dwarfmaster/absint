
{-# LANGUAGE StandaloneDeriving #-}

module Domain ( Domain( union, inter, emptyset, top
                      , singleton, singtrue, included
                      , interv, binop, unop
                      , binop_bwd, unop_bwd )
              , DomainAbstract )
       where
import AST
import Graph
import Iterator
import Data.Map (Map, (!))
import qualified Data.Map as M

-- d should be able to handle any kind of type (ie int, bool, void)
class Domain d where
    union     :: d -> d -> d
    inter     :: d -> d -> d
    emptyset  :: d
    top       :: d
    singleton :: Integer -> d -- singleton can only create an integer set,
                              -- since the only constants in the language
                              -- are integers
    singtrue  :: d -- Create the boolean set containing only the true value
    included  :: d -> d -> Bool
    interv    :: d -> d -> d
    binop     :: Binop () -> d -> d -> d
    unop      :: Unop  () -> d -> d
    binop_bwd :: Binop () -> d -> d -> d -> (d,d)
    unop_bwd  :: Unop () -> d -> d -> d

-- Wrapper transforming a domain into an abstract
data DomainAbstract d = DomainAbstract (Map EVarID d)
deriving instance Show d => Show (DomainAbstract d)

domain_bottom :: (Domain d, Ord d) => DomainAbstract d
domain_bottom = DomainAbstract M.empty

queryOr :: Ord k => v -> Map k v -> k -> v
queryOr def map key = case M.lookup key map of
                       Nothing -> def
                       Just x  -> x

eval_expr :: (Domain d, Ord d) => DomainAbstract d -> EdgeExpr -> d
eval_expr (DomainAbstract mp) (EEconst i)       = singleton i
eval_expr (DomainAbstract mp) (EEvar vid)       = queryOr emptyset mp vid
eval_expr (DomainAbstract mp) (EInter e1 e2)    = interv x1 x2
 where x1 = eval_expr (DomainAbstract mp) e1
       x2 = eval_expr (DomainAbstract mp) e2
eval_expr (DomainAbstract mp) (EEbinop b e1 e2) = binop (fmap undefined b) x1 x2
 where x1 = eval_expr (DomainAbstract mp) e1
       x2 = eval_expr (DomainAbstract mp) e2
eval_expr (DomainAbstract mp) (EEunop u e)      = unop (fmap undefined u) x
 where x  = eval_expr (DomainAbstract mp) e

domain_assign :: (Domain d, Ord d) => EVarID -> EdgeExpr -> DomainAbstract d -> DomainAbstract d
domain_assign vid expr (DomainAbstract mp) = DomainAbstract $ M.insert vid inter mp
 where inter = eval_expr (DomainAbstract mp) expr

-- TODO optimize
eval_expr_bwd :: (Domain d, Ord d) => DomainAbstract d -> d -> EdgeExpr -> DomainAbstract d
eval_expr_bwd (DomainAbstract mp) constraint (EEconst i) = if included (singleton i) constraint
                                                        then DomainAbstract mp
                                                        else domain_bottom
eval_expr_bwd (DomainAbstract mp) constraint (EEvar vid) = DomainAbstract $ M.insert vid (inter val constraint) mp
 where val = queryOr emptyset mp vid
eval_expr_bwd (DomainAbstract mp) constraint (EInter e1 e2) =
    if included (inter constraint $ interv x1 x2) emptyset then domain_bottom
                                                           else DomainAbstract mp
 where x1 = eval_expr (DomainAbstract mp) e1
       x2 = eval_expr (DomainAbstract mp) e2
eval_expr_bwd (DomainAbstract mp) constraint (EEbinop bp e1 e2) =
    eval_expr_bwd (eval_expr_bwd (DomainAbstract mp) c1 e1) c2 e2
 where x1 = eval_expr (DomainAbstract mp) e1
       x2 = eval_expr (DomainAbstract mp) e2
       (c1,c2) = binop_bwd (fmap undefined bp) x1 x2 constraint
eval_expr_bwd (DomainAbstract mp) constraint (EEunop un e) =
    eval_expr_bwd (DomainAbstract mp) c e
 where x = eval_expr (DomainAbstract mp) e
       c = unop_bwd (fmap undefined un) x constraint

domain_guard :: (Domain d, Ord d) => EdgeExpr -> DomainAbstract d -> DomainAbstract d
domain_guard expr abs = eval_expr_bwd abs singtrue expr

domain_widen :: (Domain d, Ord d) => DomainAbstract d -> DomainAbstract d
domain_widen (DomainAbstract mp) = DomainAbstract $ M.map (\_ -> top) mp

domain_subset :: (Domain d, Ord d) => DomainAbstract d -> DomainAbstract d -> Bool
domain_subset (DomainAbstract mp1) (DomainAbstract mp2) =
    M.foldWithKey (\k e b -> b && ((M.member k mp2 && included e (mp2 ! k)) || included e emptyset)) True mp1

domain_join :: (Domain d, Ord d) => DomainAbstract d -> DomainAbstract d -> DomainAbstract d
domain_join (DomainAbstract mp1) (DomainAbstract mp2) = DomainAbstract $ M.unionWith (\a1 a2 -> union a1 a2) mp1 mp2

domain_backport :: (Domain d, Ord d) => [EVarID] -> DomainAbstract d -> DomainAbstract d -> DomainAbstract d
domain_backport lst (DomainAbstract src) (DomainAbstract dst) = DomainAbstract $
    foldl (\mp id -> M.insert id (queryOr emptyset src id) mp) dst lst

instance (Domain d, Ord d) => Abstract (DomainAbstract d) where
    bottom   = domain_bottom
    assign   = domain_assign
    guard    = domain_guard
    widen    = domain_widen
    subset   = domain_subset
    join     = domain_join
    backport = domain_backport


