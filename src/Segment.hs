
module Segment where
import AST
import Domain

data GenInt = LInfinity | GInt Integer | RInfinity
data Segment = Seg GenInt GenInt

instance Show GenInt where
    show LInfinity = "-\\infty"
    show RInfinity = "+\\infty"
    show (GInt i)  = show i

instance Show Segment where
    show s | s == seg_emptyset = "_|_"
    show (Seg a b)             = "[" ++ show a ++ ", " ++ show b ++ "]"

instance Eq GenInt where
    LInfinity == LInfinity = True
    RInfinity == RInfinity = True
    (GInt i1) == (GInt i2) = i1 == i2
    _         == _         = False

instance Ord GenInt where
    LInfinity <= _         = True
    _         <= RInfinity = True
    (GInt i1) <= (GInt i2) = i1 <= i2
    _         <= _         = False

-- Priority is given to -\infty on +, and to 0 on *
instance Num GenInt where
    LInfinity + _         = LInfinity
    _         + LInfinity = LInfinity
    RInfinity + _         = RInfinity
    _         + RInfinity = RInfinity
    (GInt i1) + (GInt i2) = GInt $ i1 + i2

    LInfinity * LInfinity = RInfinity
    LInfinity * RInfinity = LInfinity
    RInfinity * RInfinity = RInfinity
    RInfinity * LInfinity = LInfinity
    LInfinity * (GInt i)  = case compare i 0 of
                             GT -> LInfinity
                             EQ -> GInt 0
                             LT -> RInfinity
    RInfinity * (GInt i)  = case compare i 0 of
                             GT -> RInfinity
                             EQ -> GInt 0
                             LT -> LInfinity
    (GInt i1) * (GInt i2) = GInt $ i1 * i2
    x         * LInfinity = LInfinity * x
    x         * RInfinity = RInfinity * x

    negate RInfinity      = LInfinity
    negate LInfinity      = RInfinity
    negate (GInt i)       = GInt $ negate i

    abs RInfinity         = RInfinity
    abs LInfinity         = RInfinity
    abs (GInt i)          = GInt $ abs i

    signum RInfinity      = GInt 1
    signum LInfinity      = GInt $ -1
    signum (GInt i)       = GInt $ signum i

    fromInteger           = GInt



valid :: Segment -> Bool
valid (Seg a b) = a <= b

seg_union :: Segment -> Segment -> Segment
seg_union (Seg a1 b1) (Seg a2 b2) = Seg (min a1 a2) (max b1 b2)

seg_inter :: Segment -> Segment -> Segment
seg_inter (Seg a1 b1) (Seg a2 b2) = if valid r then r else seg_emptyset
 where r = Seg (max a1 a2) (min b1 b2)

seg_emptyset :: Segment
seg_emptyset = Seg RInfinity LInfinity

seg_top :: Segment
seg_top = Seg LInfinity RInfinity

seg_singleton :: Integer -> Segment
seg_singleton i = Seg (GInt i) (GInt i)

seg_singtrue :: Segment
seg_singtrue = seg_singleton 1

seg_included :: Segment -> Segment -> Bool
seg_included (Seg a1 b1) (Seg a2 b2) = a2 <= a2 && b1 <= b2 

seg_interv :: Segment -> Segment -> Segment
seg_interv s1 s2 = seg_union s1 s2

mdiv :: GenInt -> GenInt -> GenInt
mdiv _         RInfinity = GInt 0
mdiv RInfinity _         = RInfinity
mdiv LInfinity _         = LInfinity
mdiv (GInt i1) (GInt i2) = GInt $ i1 `div` i2
mdiv _         LInfinity = undefined -- Will never happen

msingleton :: GenInt -> Segment
msingleton g = Seg g g

seg_binop :: Binop () -> Segment -> Segment -> Segment
seg_binop _ x _ | x == seg_emptyset = seg_emptyset
seg_binop _ _ x | x == seg_emptyset = seg_emptyset

seg_binop Bplus (Seg a1 b1) (Seg a2 b2)  = Seg (a1 + a2) (b1 + b2)
seg_binop Btimes (Seg a1 b1) (Seg a2 b2) = Seg (minimum [ a1 * a2, a1 * b2
                                                        , b1 * a2, b1 * b2])
                                               (maximum [ a1 * a2, a1 * b2
                                                        , b1 * a2, b1 * b2])
seg_binop Bless (Seg a1 b1) (Seg a2 b2) = Seg (a1 - b2) (b1 - a2)

seg_binop Bdiv (Seg a1 b1) (Seg a2 b2) | a2 > 0 = seg_union (msingleton (a1 `mdiv` a2)) $
                                                  seg_union (msingleton (b1 `mdiv` a2)) $
                                                  seg_union (msingleton (a1 `mdiv` b2)) $
                                                            (msingleton (b1 `mdiv` b2))
seg_binop Bdiv (Seg a1 b1) (Seg a2 b2) | b2 < 0 = Seg (negate b3) (negate a3)
 where Seg a3 b3 = seg_binop Bdiv (Seg a1 b1) $ Seg (negate b2) (negate a2)
seg_binop Bdiv s1 (Seg 0  b2) = seg_binop Bdiv s1 $ Seg 1 b2
seg_binop Bdiv s1 (Seg a2 0)  = seg_binop Bdiv s1 $ Seg a2 $ -1
seg_binop Bdiv s1 (Seg a2 b2) = seg_union (seg_binop Bdiv s1 (Seg a2 $ -1)) (seg_binop Bdiv s1 (Seg 1 b2))

-- Could be made more precise
seg_binop Bmod (Seg a1 b1) (Seg a2 b2) | a2 > 0 =
    if a1 > 0 then Seg 0 (b2 - (GInt 1)) else
    if b1 < 0 then Seg (negate (b2 - (GInt 1))) 0
              else Seg (negate (b2 - (GInt 1))) (b2 - (GInt 1))
seg_binop Bmod (Seg a1 b1) (Seg 0  0 ) = seg_emptyset
seg_binop Bmod (Seg a1 b1) (Seg a2 b2) = seg_binop Bmod (Seg a1 b1) $ Seg 1 $ max (abs a1) (abs b2)

seg_binop Blt (Seg a1 b1) (Seg a2 b2) =      if b1 < a2  then seg_singleton 1
                                        else if a1 >= b2 then seg_singleton 0
                                        else seg_union (seg_singleton 1) (seg_singleton 0)
seg_binop Ble (Seg a1 b1) (Seg a2 b2) =      if b1 <= a2 then seg_singleton 1
                                        else if a1 > b2  then seg_singleton 0
                                        else seg_union (seg_singleton 1) (seg_singleton 0)
seg_binop Bgt (Seg a1 b1) (Seg a2 b2) =      if b2 < a1  then seg_singleton 1
                                        else if a2 >= b1 then seg_singleton 0
                                        else seg_union (seg_singleton 1) (seg_singleton 0)
seg_binop Bge (Seg a1 b1) (Seg a2 b2) =      if b2 <= a1 then seg_singleton 1
                                        else if a2 > b1  then seg_singleton 0
                                        else seg_union (seg_singleton 1) (seg_singleton 0)
seg_binop Beq (Seg a1 b1) (Seg a2 b2) =      if a1 == b1 && b1 == a2 && a2 == b2 then seg_singleton 1
                                        else if b1 < a2 || a1 > b2               then seg_singleton 0
                                        else seg_union (seg_singleton 1) (seg_singleton 0)
seg_binop Bneq s1 s2 = seg_unop Unot $ seg_binop Beq s1 s2
seg_binop Bor  s1 s2 = if s1 == seg_singtrue || s2 == seg_singtrue
                           then seg_singleton 1
                       else if seg_included seg_singtrue s1 || seg_included seg_singtrue s2
                           then seg_union (seg_singleton 1) (seg_singleton 0)
                           else seg_singleton 0
seg_binop Band s1 s2 = if s1 == seg_singtrue && s2 == seg_singtrue
                           then seg_singleton 1
                       else if seg_included seg_singtrue s1 && seg_included seg_singtrue s2
                           then seg_union (seg_singleton 1) (seg_singleton 0)
                           else seg_singleton 0

seg_unop :: Unop () -> Segment -> Segment
seg_unop Unot (Seg (GInt i1) (GInt i2)) = Seg (GInt $ 1 - i1) (GInt $ 1 - i2)
seg_unop Unot _                         = seg_emptyset -- Only bottom is possible here as argument
seg_unop Uneg (Seg a b)                 = Seg (negate b) (negate a) -- Also works on emptyset

has_zero :: Segment -> Bool
has_zero (Seg a b) = a <= 0 && b >= 0

-- Do backward propagation on s1
binop_bwd' :: Binop () -> Segment -> Segment -> Segment -> Segment
binop_bwd' b x y z | x == seg_emptyset || y == seg_emptyset || z == seg_emptyset = seg_emptyset
binop_bwd' Bplus s1 s2 s3  = seg_inter s1 $ seg_binop Bless s3 s2
binop_bwd' Btimes s1 s2 s3 = if has_zero s2 && has_zero s3 then s1
                                                           else seg_inter s1 $ seg_binop Bdiv s3 s2 -- ???
binop_bwd' Bless s1 s2 s3  = seg_inter s1 $ seg_binop Bplus s2 s3
binop_bwd' Bdiv s1 s2 s3   = seg_inter s1 $ seg_binop Btimes s2 s3 -- ???
binop_bwd' Bmod s1 s2 s3   = s1 -- No refinement : refining would be too difficult

binop_bwd' Blt (Seg a1 b1) (Seg a2 b2) s3 | s3 == seg_singleton 1 =
    if a1 >= b2 then seg_emptyset
                else Seg a1 (min b1 (b2 - (GInt 1)))
binop_bwd' Bgt (Seg a1 b1) (Seg a2 b2) s3 | s3 == seg_singleton 1 =
    if b1 <= a2 then seg_emptyset
                else Seg (max a1 (a2 + (GInt 1))) b1
binop_bwd' Ble (Seg a1 b1) (Seg a2 b2) s3 | s3 == seg_singleton 1 =
    if a1 > b2 then seg_emptyset
               else Seg a1 (min b1 b2)
binop_bwd' Bge (Seg a1 b1) (Seg a2 b2) s3 | s3 == seg_singleton 1 =
    if b1 < a2 then seg_emptyset
               else Seg (max a1 a2) b1
binop_bwd' Beq s1 s2 s3 | s3 == seg_singleton 1 = seg_inter s1 s2
binop_bwd' Bneq (Seg a1 b1) (Seg a2 b2) s3 | s3 == seg_singleton 1 = if a1 == b1 && b1 == a2 && a2 == b2
                                                                         then seg_emptyset
                                                                     else if b1 == a2 && a2 == b2
                                                                         then Seg a1 (b1 - 1)
                                                                     else if a1 == a2 && a2 == b1
                                                                         then Seg (a1 + 1) b1
                                                                         else Seg a1 b1
binop_bwd' Bor s1 s2 s3 | s3 == seg_singleton 1 = if seg_included (seg_singleton 1) s2 then s1
                                                                                       else seg_singleton 1
binop_bwd' Band s1 s2 s3 | s3 == seg_singleton 1 = if seg_included (seg_singleton 1) s2 then seg_singleton 1
                                                                                        else seg_emptyset

binop_bwd' Blt s1 s2 s3 | s3 == seg_singleton 0 = binop_bwd' Bge s1 s2 $ seg_singleton 1
binop_bwd' Ble s1 s2 s3 | s3 == seg_singleton 0 = binop_bwd' Bgt s1 s2 $ seg_singleton 1
binop_bwd' Bgt s1 s2 s3 | s3 == seg_singleton 0 = binop_bwd' Ble s1 s2 $ seg_singleton 1
binop_bwd' Bge s1 s2 s3 | s3 == seg_singleton 0 = binop_bwd' Blt s1 s2 $ seg_singleton 1
binop_bwd' Beq s1 s2 s3 | s3 == seg_singleton 0 = binop_bwd' Bneq s1 s2 $ seg_singleton 1
binop_bwd' Bneq s1 s2 s3 | s3 == seg_singleton 0 = binop_bwd' Beq s1 s2 $ seg_singleton 1
binop_bwd' Bor s1 s2 s3  | s3 == seg_singleton 0 = if seg_included (seg_singleton 0) s2 then seg_singleton 0
                                                                                        else seg_emptyset
binop_bwd' Band s1 s2 s3  | s3 == seg_singleton 0 = if seg_included (seg_singleton 0) s2 then s1
                                                                                         else seg_singleton 0

-- We cannot deduce anything if we don't have any information concerning
-- the result of the boolean operation
binop_bwd' _ s1 _ _ = s1

-- Do backward propagation on s2
binop_bwd'' :: Binop () -> Segment -> Segment -> Segment -> Segment
binop_bwd'' Bdiv  s1 s2 s3 = s2
binop_bwd'' Bmod  s1 s2 s3 = s2
binop_bwd'' Bless s1 s2 s3 = binop_bwd' Bless s2 s1 $ seg_unop Uneg s3
binop_bwd'' Blt   s1 s2 s3 = binop_bwd' Bgt s2 s1 s3
binop_bwd'' Ble   s1 s2 s3 = binop_bwd' Bge s2 s1 s3
binop_bwd'' Bgt   s1 s2 s3 = binop_bwd' Blt s2 s1 s3
binop_bwd'' Bge   s1 s2 s3 = binop_bwd' Ble s2 s1 s3
-- All other cases are symmetric
binop_bwd'' b    s1 s2 s3 = binop_bwd' b s2 s1 s3

seg_binop_bwd :: Binop () -> Segment -> Segment -> Segment -> (Segment, Segment)
seg_binop_bwd b s1 s2 s3 = (binop_bwd' b s1 s2 s3, binop_bwd'' b s1 s2 s3)

-- Easy because Uneg and Unot are involutive
seg_unop_bwd :: Unop () -> Segment -> Segment -> Segment
seg_unop_bwd _ x y | x == seg_emptyset || y == seg_emptyset = seg_emptyset
seg_unop_bwd Uneg s1 s2 = seg_inter s1 $ seg_unop Uneg s2
seg_unop_bwd Unot s1 s2 = seg_inter s1 $ seg_unop Unot s2

instance Eq Segment where
    Seg a1 b1 == Seg a2 b2 = a1 == a2 && b1 == b2

instance Ord Segment where
    (<=) = seg_included

instance Domain Segment where
    union     = seg_union
    inter     = seg_inter
    emptyset  = seg_emptyset
    top       = seg_top
    singleton = seg_singleton
    singtrue  = seg_singtrue
    included  = seg_included
    interv    = seg_interv
    binop     = seg_binop
    unop      = seg_unop
    binop_bwd = seg_binop_bwd
    unop_bwd  = seg_unop_bwd

