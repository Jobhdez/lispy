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
  | CLet [(String, AnfExp)] AnfExp deriving Show

toanf :: Exp -> Int -> AnfExp
toanf (If (Bool x) thn els) counter =
  CExp (CIf (ABool x) (toanf thn counter) (toanf els counter))

toanf (If (Int x) thn els) counter =
  CExp (CIf (AInt x) (toanf thn counter) (toanf els counter))

toanf (If (Var x) thn els) counter =
  CExp (CIf (AVar x) (toanf thn counter) (toanf els counter))

toanf (If (If (Bool cnd) thn els) thn2 els2) counter =
  let tempName = "temp_" ++ show counter in
    CLet [(tempName, CExp (CIf (ABool cnd) (toanf thn counter) (toanf els counter)))] (toanf (If (Var tempName) thn2 els2) (counter + 1))

toanf (If (If cnd thn els) thn2 els2) counter =
  let tempName = "temp_" ++ show counter in
    let tempName2 = "temp_" ++ show (counter + 1) in
      let tempName3 = "temp_" ++ show (counter + 2) in
        case cnd of
          (If (Bool b) thn3 els3) -> CLet [(tempName, CExp (CIf (ABool b) (toanf thn3 (counter+2)) (toanf els3 (counter+2))))] (CLet [(tempName2, CExp (CIf (AVar tempName) (toanf thn (counter+3)) (toanf els (counter+3))))] (toanf (If (Var tempName2) thn2 els2) (counter + 4)))
          (If (Less a b) thn3 els3) -> CLet [(tempName, (tocomplex (Less a b)))] (CLet [(tempName2, CExp (CIf (AVar tempName) (toanf thn3 (counter + 2)) (toanf els3 (counter+2))))]  (CLet [(tempName3, CExp (CIf (AVar tempName2) (toanf thn (counter + 3)) (toanf els (counter + 3))))] (toanf (If (Var tempName3) thn2 els2) (counter + 4))))
          
toanf (Int x) counter =
  AExp (AInt x)

toanf (Bool x) counter =
  AExp (ABool x)
  
tocomplex :: Exp -> AnfExp

tocomplex (Less (Int a) (Int b)) =
 CExp (CLess (AInt a) (AInt b))

tocomplex _ = error "tocomplex: unsupported expression"
