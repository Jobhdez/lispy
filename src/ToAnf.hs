module ToAnf where

import Parser

-- takes the AST of a Scheme like language and turns it into ANF
-- which seperates atomic expressions from complex expressions

data AtomicExp = AInt Int | ABool Bool | AVar String deriving Show

data MonExp =
  AExp AtomicExp
  | MIf MonExp MonExp MonExp
  | MNot AtomicExp
  | MLess AtomicExp AtomicExp
  | MGreater AtomicExp AtomicExp
  | MPlus AtomicExp AtomicExp
  | MMinus AtomicExp AtomicExp
  | MEq AtomicExp AtomicExp
  | MNegative AtomicExp
  | MSetBang AtomicExp MonExp
  | MBegin [MonExp]
  | MWhileLoop MonExp MonExp
  | Void String
  | MLet [(AtomicExp, MonExp)] MonExp deriving Show

toanf :: Exp -> MonExp
toanf exp =
  toanf' exp 0
  where
    toanf' :: Exp -> Int -> MonExp
    toanf' (Int n) _ =
     AExp (AInt n)

    toanf' (Var n) _ =
      AExp (AVar n)

    toanf' (Bool n) _ =
      AExp (ABool n)

    toanf' (Negative a) _ =
      MNegative (AInt a)

    toanf' (Not n) counter =
      if isatomic n
      then MNot (toatomic n)
      else
        let tempName = AVar ("temp_" ++ (show counter)) in
          MLet [(tempName, toanf' n (counter+1))] (MNot tempName)
          
    toanf' (Less a b) counter =
      let tempName = AVar ("temp_" ++ (show counter)) in
        if isatomic a && isatomic b
        then
          (MLess (toatomic a) (toatomic b))
        else
          if isatomic a
          then
            MLet [(tempName, toanf' b counter)] (MLess (toatomic a) tempName)
          else
            MLet [(tempName, toanf' a counter)] (MLess tempName (toatomic b))

    toanf' (Eq a b) counter =
      let tempName = AVar ("temp_" ++ (show counter)) in
        if isatomic a && isatomic b
        then
          (MEq (toatomic a) (toatomic b))
        else
          if isatomic a
          then
            MLet [(tempName, toanf' b counter)] (MEq (toatomic a) tempName)
          else
            MLet [(tempName, toanf' a counter)] (MEq tempName (toatomic b))
            
    toanf' (Greater a b) counter =
      let tempName = AVar ("temp_" ++ show counter) in
        if isatomic a && isatomic b
        then
          (MGreater (toatomic a) (toatomic b))
        else
          if isatomic a
          then
            MLet [(tempName, toanf' b counter)] (MGreater (toatomic a) tempName)
          else
            MLet [(tempName, toanf' a counter)] (MGreater tempName (toatomic b))
            
    toanf' (Plus a b) counter =
      let tempName = AVar ("temp_" ++ show counter) in
        if isatomic a && isatomic b
        then
          (MPlus (toatomic a) (toatomic b))
        else
          if isatomic a
          then
            MLet [(tempName, toanf' b counter)] (MPlus (toatomic a) tempName)
          else
            MLet [(tempName, toanf' a counter)] (MPlus tempName (toatomic b))

    toanf' (Minus a b) counter =
      let tempName = AVar ("temp_" ++ show counter) in
        if isatomic a && isatomic b
        then
          (MMinus (toatomic a) (toatomic b))
        else
          if isatomic a
          then
            MLet [(tempName, toanf' b counter)] (MMinus (toatomic a) tempName)
          else
            MLet [(tempName, toanf' a counter)] (MMinus tempName (toatomic b))

    toanf' (Let bindings body) counter =
      MLet (bindingsToAnf bindings counter) (toanf body)
      where
        bindingsToAnf :: [(Exp, Exp)] -> Int -> [(AtomicExp, MonExp)]
        bindingsToAnf [] _ = []
        bindingsToAnf ((v, e):xs) counter =
          (toatomic v, toanf' e (counter+1)): rest
          where
            rest = bindingsToAnf xs (counter+2)
            
    toanf' (If cnd thn els) counter =
      let cndanf = toanf' cnd counter in
        case cndanf of
          AExp (ABool b) -> MIf (AExp (ABool b))  (toanf' thn counter) (toanf' els counter)
          AExp (AVar v)  -> MIf (AExp (AVar v))  (toanf' thn counter) (toanf' els counter)
          _              ->
            let tmpName = AVar ("temp_" ++ show counter) in
              (MLet [(tmpName, (toanf' cnd (counter+1)))] (MIf (AExp tmpName) (toanf' thn (counter+1)) (toanf' els (counter+1))))
                          
    toanf' (Set var exp) counter =
      MSetBang (toatomic var) (toanf' exp (counter+1))

    toanf' (Begin exps) counter =
      MBegin (beginExpsToAnf exps (counter+1))
      where
        beginExpsToAnf :: [Exp] -> Int -> [MonExp]
        beginExpsToAnf [] _ = []
        beginExpsToAnf ((e:xs)) counter =
          (toanf' e counter): rest
          where
            rest = beginExpsToAnf xs (counter+2)

    toanf' (While cnd exp) counter =
      MWhileLoop (toanf' cnd (counter+1)) (toanf' exp (counter+1))
            
isatomic :: Exp -> Bool
isatomic (Bool b) = True
isatomic (Int a) = True
isatomic (Var v) = True
isatomic _ = False

toatomic :: Exp -> AtomicExp
toatomic (Bool b) =
  (ABool b)

toatomic (Int n) =
  (AInt n)

toatomic (Var n) =
 (AVar n)
             

