{
module Parser (parseExpr, parseFile) where
import Lexer
import AST
}

%name parseExprn expr
%name parseFilen file
%tokentype { Token }
%error { happyError }
%monad { Alex }
%lexer { lexWrap } { TokenEOF _ }

%token
    intval      { TokenInt            $$ }
    int         { TokenTInt           $$ }
    void        { TokenTVoid          $$ }
    bool        { TokenTBool          $$ }
    identt      { TokenIdent          $$ }
    '&&'        { TokenAnd            $$ }
    '||'        { TokenOr             $$ }
    '!'         { TokenNot            $$ }
    if          { TokenIf             $$ }
    else        { TokenElse           $$ }
    return      { TokenReturn         $$ }
    for         { TokenFor            $$ }
    while       { TokenWhile          $$ }
    goto        { TokenGoto           $$ }
    break       { TokenBreak          $$ }
    '++'        { TokenPostIncr       $$ }
    '--'        { TokenPostDecr       $$ }
    '=='        { TokenEqual          $$ }
    '!='        { TokenNotEqual       $$ }
    '<'         { TokenLower          $$ }
    '<='        { TokenLowerEqual     $$ }
    '>'         { TokenGreater        $$ }
    '>='        { TokenGreaterEqual   $$ }
    '+'         { TokenAdd            $$ }
    '-'         { TokenSubtract       $$ }
    '*'         { TokenMultiply       $$ }
    '/'         { TokenDivide         $$ }
    '%'         { TokenModulo         $$ }
    '+='        { TokenAddAssign      $$ }
    '-='        { TokenSubtractAssign $$ }
    '*='        { TokenMultiplyAssign $$ }
    '/='        { TokenDivideAssign   $$ }
    '%='        { TokenModuloAssign   $$ }
    '='         { TokenAssign         $$ }
    '('         { TokenLParent        $$ }
    ')'         { TokenRParent        $$ }
    '{'         { TokenLBraces        $$ }
    '}'         { TokenRBraces        $$ }
    ';'         { TokenSemicolon      $$ }
    ':'         { TokenColon          $$ }
    ','         { TokenComma          $$ }

%left '||'
%left '&&'
%nonassoc '!'
%nonassoc '=' '!='
%nonassoc '>' '>=' '<' '<='
%left '+' '-'
%nonassoc NEG
%left '*' '/' '%'

%%

expr :: { Ann Expr Pos }
expr : intval                            { (Econst (fst $1), snd $1) }
     | '(' expr ')'                      { $2 }
     | expr '==' expr                    { (Ebinop $1 (Beq,          $2) $3, snd $1) }
     | expr '!=' expr                    { (Ebinop $1 (Bneq,         $2) $3, snd $1) }
     | expr '<'  expr                    { (Ebinop $1 (Blt,          $2) $3, snd $1) }
     | expr '<=' expr                    { (Ebinop $1 (Ble,          $2) $3, snd $1) }
     | expr '>'  expr                    { (Ebinop $1 (Bgt,          $2) $3, snd $1) }
     | expr '>=' expr                    { (Ebinop $1 (Bge,          $2) $3, snd $1) }
     | expr '+'  expr                    { (Ebinop $1 (Bplus,        $2) $3, snd $1) }
     | expr '-'  expr                    { (Ebinop $1 (Bless,        $2) $3, snd $1) }
     | expr '*'  expr                    { (Ebinop $1 (Btimes,       $2) $3, snd $1) }
     | expr '/'  expr                    { (Ebinop $1 (Bdiv,         $2) $3, snd $1) }
     | expr '%'  expr                    { (Ebinop $1 (Bmod,         $2) $3, snd $1) }
     | expr '&&' expr                    { (Ebinop $1 (Band,         $2) $3, snd $1) }
     | expr '||' expr                    { (Ebinop $1 (Bor,          $2) $3, snd $1) }
     | '!' expr                          { (Eunop (Unot, $1) $2, $1) }
     | '-' expr %prec NEG                { (Eunop (Uneg, $1) $2, $1) }
     | identt '(' exprlist ')'           { (Ecall (readIdent $1) $3, snd $1) }
     | identt                            { (Evar $ readIdent $1, snd $1) }

exprlist :: { [Ann Expr Pos] }
exprlist : {- empty -}        { [] }
         | expr               { [ $1 ] }
         | expr ',' exprlist  { $1 : $3 }

instr :: { Ann Instr' Pos }
instr : assignlist                          { (Iassign $1, snd $ fst $ head $1) }
      | identt '(' exprlist ')'             { (Icall (readIdent $1) $3, snd $1) }
      | return                              { (Ireturn Nothing, $1) }
      | return expr                         { (Ireturn (Just $2), $1) }
      | goto identt                         { (Igoto $ readIdent $2, $1) }
      | type decllist                       { (Idecl $1 $2, snd $1) }
      | break                               { (Ibreak, $1) }

linstr :: { Ann Instr Pos }
linstr : instr            { (Instr Nothing $1, snd $1) }
       | identt ':' instr { (Instr (Just $ readIdent $1) $3, snd $1) }

iblock :: { Ann Instr' Pos }
iblock : '{' instrlist '}' { (Iblock $2, $1) }

lblock :: { Ann Instr Pos }
lblock : iblock            { (Instr Nothing $1, snd $1) }
       | identt ':' iblock { (Instr (Just $ readIdent $1) $3, snd $1) }

control :: { Ann Instr' Pos }
control : if '(' expr ')' gen_instr                      { (Iif $3 $5 (Instr Nothing (Inop, $1), $1), $1) }
        | if '(' expr ')' gen_instr else gen_instr       { (Iif $3 $5 $7, $1) }
        | for '(' assignlist ';' expr ';' instrcomma ')' gen_instr
                                                         { (Ifor
                                                            (Instr Nothing (Iassign $3,
                                                                            snd $ fst $ head $3), $2)
                                                            $5
                                                            (Instr Nothing (Iblock $7, $6), $6)
                                                            $9, $1) }
        | while '(' expr ')' gen_instr                   { (Iwhile $3 $5, $1) }

lcontrol :: { Ann Instr Pos }
lcontrol : control             { (Instr Nothing $1, snd $1) }
         | identt ':' control { (Instr (Just $ readIdent $1) $3, snd $1) }

gen_instr :: { Ann Instr Pos }
gen_instr : linstr ';' { $1 }
          | lblock     { $1 }
          | lcontrol   { $1 }

instrlist :: { [Ann Instr Pos] }
instrlist : gen_instr           { [ $1 ] }
          | gen_instr instrlist { $1 : $2 }

instrcomma :: { [Ann Instr Pos] }
instrcomma : instr                { [ (Instr Nothing $1, snd $1) ] }
           | instr ',' instrcomma { (Instr Nothing $1, snd $1) : $3 }

assign :: { (Ann Ident Pos, Ann Expr Pos) }
assign : identt '=' expr  { (readIdent $1, $3) }
       | identt '+=' expr { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Bplus, $2) $3, snd $1)) }
       | identt '-=' expr { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Bless, $2) $3, snd $1)) }
       | identt '*=' expr { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Btimes, $2) $3, snd $1)) }
       | identt '/=' expr { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Bdiv, $2) $3, snd $1)) }
       | identt '%=' expr { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Bmod, $2) $3, snd $1)) }
       | identt '++'      { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Bplus, $2) (Econst 1, $2), snd $1)) }
       | identt '--'      { (readIdent $1, (Ebinop (Evar (Ident $ fst $1, snd $1), snd $1)
                                            (Bless, $2) (Econst 1, $2), snd $1)) }

assignlist :: { [(Ann Ident Pos, Ann Expr Pos)] }
assignlist : assign                { [ $1 ] }
           | assign ',' assignlist { $1 : $3 }

decllist :: { [(Ann Ident Pos, Maybe (Ann Expr Pos))] }
decllist : identt '=' expr                  { [(readIdent $1, Just $3)] }
         | identt                           { [(readIdent $1, Nothing)] }
         | identt '=' expr ',' decllist     { (readIdent $1, Just $3) : $5 }
         | identt ',' decllist              { (readIdent $1, Nothing) : $3 }

type :: { Ann Type Pos }
type : void  { (Tvoid, $1) }
     | int   { (Tint,  $1) }
     | bool  { (Tbool, $1) }

toplevel :: { Ann TopLevel Pos }
toplevel : type decllist ';'                       { (TdeclVar $1 $2, snd $1) }
         | type identt '(' paramlist ')' ';'       { (TdeclFun $1 (readIdent $2) $4, snd $1) }
         | type identt '(' paramlist ')' gen_instr { (TimplFun $1 (readIdent $2) $4 $6, snd $1) }

paramlist :: { [(Ann Ident Pos, Ann Type Pos)] }
paramlist : {- empty -}               { [] }
          | type identt               { [(readIdent $2, $1)] }
          | type identt ',' paramlist { (readIdent $2, $1) : $4 }

file :: { File }
file : {- empty -}   { [] }
     | toplevel file { $1 : $2 }

{

readIdent :: (String, Pos) -> Ann Ident Pos
readIdent (s, p) = (Ident s, p)

lexWrap :: (Token -> Alex a) -> Alex a
lexWrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError t = alexError' ("parse error at token `" ++ show t ++ "`")

parseExpr :: String -> FilePath -> Either String (Ann Expr Pos)
parseExpr s fp = runAlex' s fp parseExprn

parseFile :: String -> FilePath -> Either String File
parseFile s fp = runAlex' s fp parseFilen

}
