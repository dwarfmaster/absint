
module AST where
import Data.List (intercalate)

data Pos = Pos { pos_file   :: String
               , pos_line   :: Int
               , pos_column :: Int
               } deriving (Eq, Show)

type Ann a b = (a b, b)

data Ident a = Ident String
instance Show (Ident a) where
    show (Ident s) = s

data Binop a =
      Bplus
    | Btimes
    | Bless
    | Bdiv
    | Bmod
    | Blt
    | Ble
    | Beq
    | Bneq
    | Bgt
    | Bge
    | Bor
    | Band
instance Show (Binop a) where
    show Bplus  = "+"
    show Btimes = "*"
    show Bless  = "-"
    show Bdiv   = "/"
    show Bmod   = "%"
    show Blt    = "<"
    show Ble    = "<="
    show Beq    = "=="
    show Bneq   = "!="
    show Bgt    = ">"
    show Bge    = ">="
    show Bor    = "||"
    show Band   = "&&"

data Unop a =
      Uneg
    | Unot
instance Show (Unop a) where
    show Uneg = "-"
    show Unot = "!"

data Expr a =
      Econst Integer
    | Evar   (Ann Ident a)
    | Ecall  (Ann Ident a) [Ann Expr a]
    | Ebinop (Ann Expr a) (Ann Binop a) (Ann Expr a)
    | Eunop  (Ann Unop a) (Ann Expr a)
instance Show (Expr a) where
    show (Econst v)             = show v
    show (Evar (Ident s, _))    = s
    show (Ecall (Ident s, _) l) = s ++ "(" ++ (intercalate ", " $ map (show . fst) l) ++ ")"
    show (Ebinop e1 b e2)       =      "(" ++ show (fst e1) ++ ") " ++ show (fst b)
                                   ++ " (" ++ show (fst e2) ++ ")"
    show (Eunop u e)            = show (fst u) ++ "(" ++ show (fst e) ++ ")"

data Type a =
      Tint
    | Tvoid
    | Tbool
instance Show (Type a) where
    show Tint  = "int"
    show Tvoid = "void"
    show Tbool = "bool"

data Instr' a =
      Iblock   [Ann Instr a]
    | Inop
    | Ibreak
    | Idecl    (Ann Type a) [(Ann Ident a, Maybe (Ann Expr a))]
    | Iassign  [(Ann Ident a, Ann Expr a)]
    | Icall    (Ann Ident a) [Ann Expr a]
    | Iif      (Ann Expr a) (Ann Instr a) (Ann Instr a) -- If there is no else, the second instruction is Inop
    | Iwhile   (Ann Expr a) (Ann Instr a)
    | Ifor     (Ann Instr a) (Ann Expr a) (Ann Instr a) (Ann Instr a)
    | Igoto    (Ann Ident a)
    | Ireturn  (Maybe (Ann Expr a))
indent :: Int -> String
indent n = concat $ take n $ repeat "\t"
format' :: Int -> Instr' a -> String
format' n (Iblock lst)   = (indent n) ++ "{\n"
                           ++ (concatMap ((++ "\n") . format (n + 1) . fst) lst)
                           ++ (indent n) ++ "}"
format' n Inop           = (indent n) ++ ";"
format' n Ibreak         = (indent n) ++ "break;"
format' n (Idecl t dcls) = (indent n) ++ show (fst t) ++ " " ++
                           (intercalate ", " $ map showDecl dcls) ++ ";"
 where showDecl ((Ident s, _), Nothing) = s
       showDecl ((Ident s, _), Just e)  = s ++ " = " ++ (show $ fst e)
format' n (Iassign l) = (indent n) ++ (intercalate ", " $ map showAssign l) ++ ";"
 where showAssign ((Ident s, _), e) = s ++ " = " ++ (show $ fst e)
format' n (Icall (Ident f, _) params) = (indent n) ++ f ++ "("
                                        ++ (intercalate ", " $ map (show . fst) params)
                                        ++ ");"
format' n (Iif ec i1 i2) = (indent n) ++ "if(" ++ (show $ fst ec) ++ ")\n" 
                           ++ (format (n+1) $ fst i1) ++ "\n"
                           ++ (indent n) ++ "else\n"
                           ++ (format (n+1) $ fst i2)
format' n (Iwhile e i) = (indent n) ++ "while(" ++ (show $ fst e) ++ ")\n"
                         ++ (format (n+1) $ fst i)
format' n (Ifor i1 e i2 bd) = (indent n) ++ "for(" ++ (show $ fst i1) ++ (show $ fst e) ++ ";"
                              ++ (show $ fst i2) ++ ")\n" ++ (show $ fst bd)
format' n (Igoto (Ident s, _)) = (indent n) ++ "goto " ++ s ++ ";"
format' n (Ireturn Nothing) = (indent n) ++ "return;"
format' n (Ireturn (Just e)) = (indent n) ++ "return " ++ (show $ fst e) ++ ";"
instance Show (Instr' a) where
    show = format' 0

-- Handle labels before instructions
data Instr a = Instr (Maybe (Ann Ident a)) (Ann Instr' a)
format :: Int -> Instr a -> String
format n (Instr Nothing (i, _)) = format' n i
format n (Instr (Just (Ident l,_)) (i, _)) = format' n i ++ "[" ++ l ++ "]"
instance Show (Instr a) where
    show = format 0

data TopLevel a =
      TdeclVar (Ann Type a) [(Ann Ident a, Maybe (Ann Expr a))]
    | TdeclFun (Ann Type a) (Ann Ident a) [(Ann Ident a, Ann Type a)]
    | TimplFun (Ann Type a) (Ann Ident a)
               [(Ann Ident a, Ann Type a)] (Ann Instr a)
instance Show (TopLevel a) where
    show (TdeclVar t l) = show $ Idecl t l
    show (TdeclFun tp (Ident nm, _) prms) = (show $ fst tp) ++ nm ++ "(" ++
                                            (intercalate ", "
                                                $ map (\((Ident n, _), (t,_)) -> (show t) ++ " " ++ n) prms)
                                            ++ ")"
    show (TimplFun tp (Ident nm, _) prms bd) = (show $ fst tp) ++ nm ++ "(" ++
                                               (intercalate ", "
                                                   $ map (\((Ident n, _), (t,_)) -> (show t) ++ " " ++ n)
                                                         prms)
                                               ++ ")\n\t" ++ (show $ fst bd)

type File = [Ann TopLevel Pos]
showFile :: File -> String
showFile = (intercalate "\n") . (map $ show . fst)

