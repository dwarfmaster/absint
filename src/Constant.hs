
module Constant (ConstantDomain) where
import Utility
import AST
import Domain

data ConstantDomain = Top | Bottom | Const Integer
instance Show ConstantDomain where
    show Top       = "|N"
    show Bottom    = "_|_"
    show (Const i) = "{" ++ show i ++ "}"

cdom_union :: ConstantDomain -> ConstantDomain -> ConstantDomain
cdom_union Top _ = Top
cdom_union _ Top = Top
cdom_union x Bottom = x
cdom_union Bottom x = x
cdom_union (Const i1) (Const i2) = if i1 == i2 then Const i1 else Top

cdom_inter :: ConstantDomain -> ConstantDomain -> ConstantDomain
cdom_inter Top x = x
cdom_inter x Top = x
cdom_inter Bottom _ = Bottom
cdom_inter _ Bottom = Bottom
cdom_inter (Const i1) (Const i2) = if i1 == i2 then Const i1 else Bottom

cdom_emptyset :: ConstantDomain
cdom_emptyset = Bottom

cdom_top :: ConstantDomain
cdom_top = Top

cdom_singleton :: Integer -> ConstantDomain
cdom_singleton i = Const i

cdom_singtrue :: ConstantDomain
cdom_singtrue = Const 1

cdom_included :: ConstantDomain -> ConstantDomain -> Bool
cdom_included _ Top = True
cdom_included Top _ = False
cdom_included Bottom _ = True
cdom_included _ Bottom = False
cdom_included (Const i1) (Const i2) = if i1 == i2 then True else False

cdom_interv :: ConstantDomain -> ConstantDomain -> ConstantDomain
cdom_interv _ Bottom = Bottom
cdom_interv Bottom _ = Bottom
cdom_interv (Const i1) (Const i2) = if i1 == i2 then Const i1 else Top
cdom_interv _ _ = Top

cdom_binop :: Binop () -> ConstantDomain -> ConstantDomain -> ConstantDomain
cdom_binop _ Bottom _ = Bottom
cdom_binop _ _ Bottom = Bottom
cdom_binop _ _ Top    = Top
cdom_binop _ Top _    = Top
cdom_binop b (Const i1) (Const i2) = if i2 == 0 && dodiv b then Bottom else Const $ interpret_binop b i1 i2

cdom_unop :: Unop () -> ConstantDomain -> ConstantDomain
cdom_unop _ Bottom = Bottom
cdom_unop _ Top    = Top
cdom_unop u (Const i) = Const $ interpret_unop u i

cdom_binop_bwd :: Binop () -> ConstantDomain -> ConstantDomain -> ConstantDomain -> (ConstantDomain,ConstantDomain)
cdom_binop_bwd _ x y Top    = (x,y)
cdom_binop_bwd _ _ _ Bottom = (Bottom,Bottom)
-- It could be a bit more clever here : in case of patterns like Top + Const i = Const j,
-- it could deduce that Top -> Const (i - j). It is not difficult to add, but since it
-- cannot be systematized, I don't have time to do it for now.
cdom_binop_bwd _ Top y _    = (Top, y)
cdom_binop_bwd _ x Top _    = (x, Top)
cdom_binop_bwd _ Bottom _ _ = (Bottom, Bottom)
cdom_binop_bwd _ _ Bottom _ = (Bottom, Bottom)
cdom_binop_bwd b (Const i1) (Const i2) (Const i) = if interpret_binop b i1 i2 == i then (Const i1, Const i2)
                                                                                   else (Bottom, Bottom)

cdom_unop_bwd :: Unop () -> ConstantDomain -> ConstantDomain -> ConstantDomain
cdom_unop_bwd _ x Top    = x
cdom_unop_bwd _ _ Bottom = Bottom
-- Same as above, it could be made a bit more clever here with more time
cdom_unop_bwd _ Top _    = Top
cdom_unop_bwd _ Bottom _ = Bottom
cdom_unop_bwd u (Const i) (Const j) = if interpret_unop u i == j then Const i else Bottom

instance Domain ConstantDomain where
    union     = cdom_union
    inter     = cdom_inter
    emptyset  = cdom_emptyset
    top       = cdom_top
    singleton = cdom_singleton
    singtrue  = cdom_singtrue
    included  = cdom_included
    interv    = cdom_interv
    binop     = cdom_binop
    unop      = cdom_unop
    binop_bwd = cdom_binop_bwd
    unop_bwd  = cdom_unop_bwd

instance Eq ConstantDomain where
    Top       == Top       = True
    Bottom    == Bottom    = True
    (Const i) == (Const j) = i == j
    _         == _         = False

instance Ord ConstantDomain where
    Bottom <= _   = True
    _      <= Top = True
    _      <= _   = False
