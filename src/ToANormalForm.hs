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
  | CLet [(AtomicExp, AnfExp)] AnfExp deriving Show

  
toanf :: Exp -> Int -> AnfExp
toanf (If (Bool x) thn els) counter =
  CExp (CIf (ABool x) (toanf thn counter) (toanf els counter))

toanf (If (Var x) thn els) counter =
  CExp (CIf (AVar x) (toanf thn counter) (toanf els counter))
  
toanf (If (Less a b) thn els) counter =
  let tempName = AVar ("temp_" ++ show counter) in
    CLet [(tempName, tocomplex (Less a b))] (toanf (If (Var ("temp_" ++ show counter)) thn els) (counter + 1))
    
toanf (If (If (Bool cnd) thn els) thn2 els2) counter =
  let tempName = AVar ("temp_" ++ show counter) in
    CLet [(tempName, CExp (CIf (ABool cnd) (toanf thn counter) (toanf els counter)))] (toanf (If (Var ("temp_" ++ show counter)) thn2 els2) (counter + 1))

toanf (If (If cnd thn els) thn2 els2) counter =
  let tempName = AVar ("temp_" ++ show counter) in
    let tempName2 = AVar ("temp_" ++ show (counter + 1)) in
      let tempName3 = AVar ("temp_" ++ show (counter + 2)) in
        case cnd of
          (If (Bool b) thn3 els3) -> CLet [(tempName, CExp (CIf (ABool b) (toanf thn3 (counter+2)) (toanf els3 (counter+2))))] (CLet [(tempName2, CExp (CIf (AVar ("temp_" ++ show counter)) (toanf thn (counter+3)) (toanf els (counter+3))))] (toanf (If (Var ("temp_" ++ show (counter + 1))) thn2 els2) (counter + 4)))
          (If (Less a b) thn3 els3) -> CLet [(tempName, (tocomplex (Less a b)))] (CLet [(tempName2, CExp (CIf (AVar ("temp_" ++ show counter)) (toanf thn3 (counter + 2)) (toanf els3 (counter+2))))]  (CLet [(tempName3, CExp (CIf (AVar ("temp_" ++ show (counter + 1))) (toanf thn (counter + 3)) (toanf els (counter + 3))))] (toanf (If (Var ("temp_" ++ show (counter + 2))) thn2 els2) (counter + 4))))
   
toanf (Int x) counter =
  AExp (AInt x)

toanf (Bool x) counter =
  AExp (ABool x)

toanf (Let [(Var x, Int y)] (Int z)) counter =
  (CLet [(AVar x, AExp (AInt y))] (AExp (AInt z)))

toanf (Let [(Var x, Var y)] (Int z)) counter =
  CLet [(AVar x, AExp (AVar y))] (AExp (AInt z))

toanf (Let [(Var x, If (Bool b) thn els)] body) counter =
  CLet [(AVar x, CExp (CIf (ABool b) (toanf thn counter) (toanf els counter)))] (toanf body counter)

toanf (Let [(Var x, If (Less a b) thn els)] body) counter =
  let tempName = AVar ("temp_" ++ show counter) in
    CLet [(tempName, (tocomplex (Less a b)))] (CLet [(AVar x, CExp (CIf (AVar ("temp_" ++ show counter)) (toanf thn (counter + 1)) (toanf els (counter + 1))))] (toanf body (counter + 1)))

toanf (Let [(Var x, (Let [(Var v, Int e)] body))] body2) counter =
  CLet [(AVar v, AExp (AInt e))] (CLet [(AVar x, tocomplex body)] (toanf body2 counter))

toanf (Plus (Var v) (Int x)) counter =
  CExp (CPlus (AVar v) (AInt x))

toanf (Plus (Var v) (Var v2)) counter =
  CExp (CPlus (AVar v) (AVar v2))

toanf (Let [(Var x, Int y)] (Let [(Var v, Int z)] body)) counter =
  CLet [(AVar x, AExp (AInt y))] (CLet [(AVar v, AExp (AInt z))] (toanf body counter))

toanf (Let [(Var v, Int x)] body) counter =
  (CLet [(AVar v, AExp (AInt x))] (toanf body counter))
  
toanf exp counter =
  toanf exp counter
  
tocomplex :: Exp -> AnfExp
tocomplex (Less (Int a) (Int b)) =
 CExp (CLess (AInt a) (AInt b))

tocomplex (Less (Var a) (Int b)) =
 CExp (CLess (AVar a) (AInt b))

tocomplex (Plus (Int a) (Int b)) =
  CExp (CPlus (AInt a) (AInt b))

tocomplex (Plus (Var a) (Int b)) =
  CExp (CPlus (AVar a) (AInt b))

tocomplex (Plus (Var v) (Var v2)) =
  CExp (CPlus (AVar v) (AVar v2))
  
tocomplex _ = error "tocomplex: unsupported expression"
