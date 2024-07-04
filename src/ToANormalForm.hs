module ToANormalForm where

import Parser

isAtomic :: Exp -> Bool
isAtomic (Bool b) = True
isAtomic (Int a) = True
isAtomic (Var v) = True
isAtomic _ = False

data AtomicExp = AInt Int | ABool Bool | AVar String deriving Show

data ComplexExp =
  CIf AtomicExp AnfExp AnfExp
  | CSet Exp AnfExp deriving Show
  
data AnfExp =
  AExp AtomicExp
  | CExp ComplexExp
  | Let [(String, ComplexExp)] AnfExp deriving Show
  
toanf :: Exp -> AnfExp
toanf (If (Bool x) thn els) =
  CExp (CIf (ABool x) (toanf thn) (toanf els))

toanf (If (Int x) thn els) =
  CExp (CIf (AInt x) (toanf thn) (toanf els))

toanf (If (Var x) thn els) =
  CExp (CIf (AVar x) (toanf thn) (toanf els))
  
toanf (Int x) =
  AExp (AInt x)
