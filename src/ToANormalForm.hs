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
  | CLess AtomicExp AtomicExp
  | CPlus AtomicExp AtomicExp deriving Show
data AnfExp =
  AExp AtomicExp
  | CExp ComplexExp
  | CLet [(String, AnfExp)] AnfExp deriving Show

  
toanf :: Exp -> Int -> AnfExp
toanf (If (Bool x) thn els) counter =
  CExp (CIf (ABool x) (toanf thn counter) (toanf els counter))

toanf (If (Var x) thn els) counter =
  CExp (CIf (AVar x) (toanf thn counter) (toanf els counter))
toanf (If (Less a b) thn els) counter =
  let tempName = "temp_" ++ show counter in
    CLet [(tempName, tocomplex (Less a b))] (toanf (If (Var tempName) thn els) (counter + 1))
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

toanf (Let [(Var x, Int y)] (Int z)) counter =
  (CLet [(x, AExp (AInt y))] (AExp (AInt z)))

toanf (Let [(Var x, Var y)] (Int z)) counter =
  CLet [(x, AExp (AVar y))] (AExp (AInt z))

toanf (Let [(Var x, If (Bool b) thn els)] body) counter =
  CLet [(x, CExp (CIf (ABool b) (toanf thn counter) (toanf els counter)))] (toanf body counter)

toanf (Let [(Var x, If (Less a b) thn els)] body) counter =
  let tempName = "temp_" ++ show counter in
    CLet [(tempName, (tocomplex (Less a b)))] (CLet [(x, CExp (CIf (AVar tempName) (toanf thn (counter + 1)) (toanf els (counter + 1))))] (toanf body (counter + 1)))

toanf (Let [(Var x, (Let [(Var v, Int e)] body))] body2) counter =
  CLet [(v, AExp (AInt e))] (CLet [(x, tocomplex body)] (toanf body2 counter))

toanf (Plus (Var v) (Int x)) counter =
  CExp (CPlus (AVar v) (AInt x))
tocomplex :: Exp -> AnfExp
tocomplex (Less (Int a) (Int b)) =
 CExp (CLess (AInt a) (AInt b))

tocomplex (Plus (Int a) (Int b)) =
  CExp (CPlus (AInt a) (AInt b))

tocomplex (Plus (Var a) (Int b)) =
  CExp (CPlus (AVar a) (AInt b))
tocomplex _ = error "tocomplex: unsupported expression"
