
module Concrete(IntSet) where
import Domain
import AST
import Utility
import Data.Set (Set, (\\))
import qualified Data.Set as S

-- Boolean sets are only integer sets with 0 and/or 1 inside
data IntSet = Finite (Set Integer) | Top
    deriving (Show)

set_union :: IntSet -> IntSet -> IntSet
set_union Top _ = Top
set_union _ Top = Top
set_union (Finite s1) (Finite s2) = Finite $ S.union s1 s2

set_inter :: IntSet -> IntSet -> IntSet
set_inter Top x = x
set_inter x Top = x
set_inter (Finite s1) (Finite s2) = Finite $ S.intersection s1 s2

set_emptyset :: IntSet
set_emptyset = Finite S.empty

set_top :: IntSet
set_top = Top

set_singleton :: Integer -> IntSet
set_singleton i = Finite $ S.singleton i

set_singtrue :: IntSet
set_singtrue = Finite $ S.singleton 1

set_included :: IntSet -> IntSet -> Bool
set_included _ Top = True
set_included Top _ = False
set_included (Finite s1) (Finite s2) = S.isSubsetOf s1 s2

set_interv :: IntSet -> IntSet -> IntSet
set_interv _ Top = Top
set_interv Top _ = Top
set_interv (Finite s1) (Finite s2) = if S.null s1 || S.null s2 then set_emptyset
                                                               else Finite $ S.fromList [min .. max]
 where min = S.findMin s1
       max = S.findMin s2
       
haszero :: Set Integer -> Bool
haszero s = (not $ S.null s) && S.findMin s <= 0 && S.findMax s >= 0

set_binop :: Binop () -> IntSet -> IntSet -> IntSet
set_binop b Top Top                 = if dodiv b then set_emptyset else Top
set_binop b Top (Finite s)          = if (dodiv b && haszero s) || S.null s then set_emptyset else Top
set_binop b (Finite s) Top          = if dodiv b || S.null s then set_emptyset else Top
set_binop b (Finite s1) (Finite s2) = if dodiv b && haszero s2 then set_emptyset
                                      else Finite $ S.fromList [x `op` y | x <- S.toList s1, y <- S.toList s2]
 where op = interpret_binop b

set_unop :: Unop () -> IntSet -> IntSet
set_unop u Top = Top
set_unop u (Finite s) = Finite $ S.map (interpret_unop u) s

binop_bwd_oneway :: (Integer -> Integer -> Integer) -> IntSet -> IntSet -> IntSet -> IntSet
binop_bwd_oneway b is1         is2         Top        = is1
binop_bwd_oneway b is1         Top         (Finite s) = if S.null s then set_emptyset else is1
-- Rough approximation, but any better approximation would be too computationally expensive
binop_bwd_oneway b Top         (Finite s2) (Finite s) = if S.null s || S.null s2 then set_emptyset else Top
binop_bwd_oneway b (Finite s1) (Finite s2) (Finite s) =
    if S.null s || S.null s2 then set_emptyset
                             else Finite $ S.filter filter s1
 where filter i = not $ S.null $ S.intersection s $ S.map (b i) s2

set_binop_bwd :: Binop () -> IntSet -> IntSet -> IntSet -> (IntSet, IntSet)
set_binop_bwd b is1 is2 is = (binop_bwd_oneway op is1 is2' is, binop_bwd_oneway (flip op) is2' is1 is)
 where op = interpret_binop b
       is2' = if not $ dodiv b then is2
                               else case is2 of
                                     Top      -> Top
                                     Finite s -> Finite $ s \\ S.singleton 0

set_unop_bwd :: Unop () -> IntSet -> IntSet -> IntSet
set_unop_bwd u is1         Top        = is1
-- Rough approximation, but any better approximation would be too computationally expensive
set_unop_bwd u Top         _          = Top
set_unop_bwd u (Finite s1) (Finite s) = Finite $ S.filter filter s1
 where filter i = S.member (interpret_unop u i) s

instance Domain IntSet where
    union     = set_union
    inter     = set_inter
    emptyset  = set_emptyset
    top       = set_top
    singleton = set_singleton
    singtrue  = set_singtrue
    included  = set_included
    interv    = set_interv
    binop     = set_binop
    unop      = set_unop
    binop_bwd = set_binop_bwd
    unop_bwd  = set_unop_bwd

instance Eq IntSet where
    Top         == Top         = True
    (Finite s1) == (Finite s2) = s1 == s2
    _           == _           = False

instance Ord IntSet where
    (<=) = set_included

