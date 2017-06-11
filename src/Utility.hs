
module Utility where
import AST

dodiv :: Binop () -> Bool
dodiv Bdiv = True
dodiv Bmod = True
dodiv _    = False

fromBool :: (Integer -> Integer -> Bool) -> (Integer -> Integer -> Integer)
fromBool r = \x y -> f $ r x y
 where f True  = 1
       f False = 0

fromBBBool :: (Bool -> Bool -> Bool) -> (Integer -> Integer -> Integer)
fromBBBool r = \x y -> f $ r (g x) (g y)
 where f True  = 1
       f False = 0
       g i = i /= 0

interpret_binop :: Binop () -> Integer -> Integer -> Integer
interpret_binop Bplus  = (+)
interpret_binop Btimes = (*)
interpret_binop Bless  = (-)
interpret_binop Bdiv   = div
interpret_binop Bmod   = mod
interpret_binop Blt    = fromBool (<)
interpret_binop Ble    = fromBool (<=)
interpret_binop Beq    = fromBool (==)
interpret_binop Bneq   = fromBool (/=)
interpret_binop Bgt    = fromBool (>)
interpret_binop Bge    = fromBool (>=)
interpret_binop Bor    = fromBBBool (||)
interpret_binop Band   = fromBBBool (&&)

interpret_unop :: Unop () -> Integer -> Integer
interpret_unop Uneg x = -x
interpret_unop Unot x = f $ not $ x /= 0
 where f True  = 1
       f False = 0

