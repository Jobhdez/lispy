{
module Parser where

import Data.Char (isSpace, isAlpha, isDigit)
}

%name parseExp
%tokentype { Token }
%error { parseError }

%token
if            { TokenIf }
let           { TokenLet }
while         { TokenWhile }
var           { TokenVar $$ }
int           { TokenInt $$ }
t             { TokenTrue }
f             { TokenFalse }
set           { TokenSet }
begin         { TokenBegin }
vectorref     { TokenTupleRef }
vector        { TokenVector }
vectorlength  { TokenVecLength }
and           { TokenAnd }
or            { TokenOr }
not           { TokenNot }
eq            { TokenEq }
'+'           { TokenPlus }
'-'           { TokenMinus }
'<'           { TokenLess }
'>'           { TokenGreater }
')'           { TokenRParen }
'('           { TokenLParen }
%%

Exp : var { Var $1 }
    | '(' let '(' Bindings ')' Exp ')' { Let $4 $6}
    | '(' set var Exp ')' { Set (Var $3) $4 }
    | '(' begin Exps ')' { Begin $3 }
    | '(' if Exp Exp Exp ')' { If $3 $4 $5 }
    | '(' while Exp Exp ')' { While $3 $4 }
    | t { Bool True }
    | f { Bool False }
    | '(' vectorref Exp int ')' { VectorRef $3 $4 }
    | '(' vector Exps ')' { Vector $3 }
    | '(' vectorlength Exp ')' { VectorLength $3 }
    | '(' and Exp Exp ')' { And $3 $4 }
    | '(' or Exp Exp ')' { Or $3 $4 }
    | '(' not Exp ')' { Not $3 }
    | '(' eq Exp Exp ')' { Eq $3 $4 }
    | '(' '+' Exp Exp ')' { Plus $3 $4 }
    | '(' '-' Exp Exp ')' { Minus $3 $4 }
    | '(' '<' Exp Exp ')' { Less $3 $4 }
    | '(' '>' Exp Exp ')' { Greater $3 $4 }
    | int { Int $1 }
    | '(' '-' int ')' { Negative $3 }

Exps : Exp { [$1] }
     | Exp Exps { $1 : $2 }

Binding : '(' Exp Exp ')' { ($2, $3) }

Bindings : Binding { [$1] }
         | Binding Bindings { $1 : $2 }

{
data Token
  = TokenIf
  | TokenLet
  | TokenWhile
  | TokenVar String
  | TokenInt Int
  | TokenTrue
  | TokenFalse
  | TokenSet
  | TokenBegin
  | TokenTupleRef
  | TokenVector
  | TokenVecLength
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenLess
  | TokenGreater
  | TokenRParen
  | TokenLParen
  deriving (Show)

data Exp
  = Var String
  | Let [(Exp, Exp)] Exp 
  | Set Exp Exp
  | Begin [Exp]
  | If Exp Exp Exp
  | While Exp Exp
  | Bool Bool
  | VectorRef Exp Int
  | Vector [Exp]
  | VectorLength Exp
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  | Eq Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | Less Exp Exp
  | Greater Exp Exp
  | Int Int
  | Negative Int
  deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexExp (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenGreater : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
  where (num, rest) = span isDigit cs

lexExp cs =
  case span isAlpha cs of
  ("if", rest) -> TokenIf : lexer rest
  ("let", rest) -> TokenLet : lexer rest
  ("while", rest) -> TokenWhile : lexer rest
  ("t", rest) -> TokenTrue : lexer rest
  ("f", rest) -> TokenFalse : lexer rest
  ("set", rest) -> TokenSet : lexer rest
  ("begin", rest) -> TokenBegin : lexer rest
  ("eq", rest) -> TokenEq : lexer rest
  ("not", rest) -> TokenNot : lexer rest 
  (var, rest) -> TokenVar var : lexer rest

main = getContents >>= print . parseExp . lexer
}
