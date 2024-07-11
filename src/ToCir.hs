module ToCir where

import Parser 
import ToAnf

data Cir =
  CInt Int
  | CVar String
  | CPlus Cir Cir
  | CReturn Cir
  | CLess Cir Cir
  | IfStmt Cir Cir Cir
  | Assign Cir Cir deriving Show
  
tocir :: MonExp -> [Cir]
tocir (MLet [(AVar var, AExp (AInt x))] (AExp (AInt y))) =
  [Assign (CVar var) (CInt x), CReturn (CInt y)]

tocir (MLet [(AVar var, AExp (AInt x))] body) =
  [(Assign (CVar var) (CInt x))] ++ tocir body

tocir (MLet [(AVar var, (MLet [(v, exp)] body))] body2) =
  tocir (MLet [(v, exp)] body) ++ [Assign (CVar var) (head (tocir body2))]

tocir (MLet [(AVar var, MLess (AVar x) (AInt y))] body) =
  [Assign (CVar var) (head (tocir (MLess (AVar x) (AInt y))))] ++ tocir body

tocir (MLet [(AVar var, MLess (AInt y) (AInt z))] body) =
  [Assign (CVar var) (head (tocir (MLess (AInt y) (AInt z))))] ++ tocir body
  
tocir (MLet [(AVar var, exp)] body) =
  tocir exp ++ tocir body

tocir (MPlus (AVar a) (AVar b)) =
  [CPlus (CVar a) (CVar b)]

tocir (MLess (AVar x) (AInt y)) =
  [CLess (CVar x) (CInt y)]

tocir (MLess (AInt x) (AInt y)) =
  [CLess (CInt x) (CInt y)]

tocir (MPlus (AVar a) (AInt b)) =
  [CPlus (CVar a) (CInt b)]

tocir (AExp (AInt b)) =
  [CInt b]

tocir (MIf (AExp (AVar tmp)) thn els) =
  [IfStmt (CVar tmp) (head (tocir thn)) (head (tocir els))]
  
tocir (AExp (AVar b)) =
  [CVar b]

tocir' :: [Cir] -> [Cir]
tocir' [] = []
tocir' (x:xs) =
  case x of
    IfStmt cnd thn els ->
      if null xs
      then x : tocir' []
      else
        case head xs of
          Assign var (IfStmt cnd2 thn2 els2) ->
            (IfStmt cnd (Assign var thn) (Assign var els)) : tocir' xs
          _ -> x : tocir' xs
    Assign var (IfStmt cnd2 thn2 els2) ->
      if null xs 
      then x : tocir' []
      else
        case head xs of
          Assign var2 (IfStmt cnd3 thn3 els3) ->
            (IfStmt cnd2 (Assign var2 thn2) (Assign var2 els2)) : (IfStmt cnd3 thn3 els3) : tocir' xs
          _ -> x : tocir' xs
    _ -> x : tocir' xs
