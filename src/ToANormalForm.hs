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
  | CSet Exp AnfExp
  | CLess AtomicExp AtomicExp deriving Show

data AnfExp =
  AExp AtomicExp
  | CExp ComplexExp
  | CLet [(String, ComplexExp)] AnfExp deriving Show

toanf :: Exp -> Int -> AnfExp
toanf (If (Bool x) thn els) counter =
  CExp (CIf (ABool x) (toanf thn counter) (toanf els counter))

toanf (If (Int x) thn els) counter =
  CExp (CIf (AInt x) (toanf thn counter) (toanf els counter))

toanf (If (Var x) thn els) counter =
  CExp (CIf (AVar x) (toanf thn counter) (toanf els counter))

toanf (If (If (Bool cnd) thn els) thn2 els2) counter =
  let tempName = "temp_" ++ show counter in
    CLet [(tempName, CIf (ABool cnd) (toanf thn counter) (toanf els counter))] (toanf (If (Var tempName) thn2 els2) (counter + 1))

toanf (If (If cnd thn els) thn2 els2) counter =
  let tempName = "temp_" ++ show counter in
  let tempName2 = "temp_" ++ show (counter + 1) in
    CLet [(tempName, (tocomplex cnd))] (CLet [(tempName2, (CIf (AVar tempName) (toanf thn (counter + 2)) (toanf els (counter + 2))))] (toanf (If (Var tempName2) thn2 els2) (counter + 2)))

toanf (Int x) counter =
  AExp (AInt x)

toanf (Bool x) counter =
  AExp (ABool x) 
tocomplex :: Exp -> ComplexExp
tocomplex (Less (Int a) (Int b)) =
  CLess (AInt a) (AInt b)

tocomplex _ = error "tocomplex: unsupported expression"
