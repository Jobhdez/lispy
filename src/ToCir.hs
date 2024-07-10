module ToCir where

import Parser 
import ToAnf

data Cir =
  CInt Int
  | CVar String
  | CPlus Cir Cir
  | CReturn Cir
  | Assign Cir Cir deriving Show
  
tocir :: MonExp -> [Cir]
tocir (MLet [(AVar var, AExp (AInt x))] (AExp (AInt y))) =
  [Assign (CVar var) (CInt x), CReturn (CInt y)]

tocir (MLet [(AVar var, AExp (AInt x))] body) =
  [(Assign (CVar var) (CInt x))] ++ tocir body

tocir (MLet [(AVar var, (MLet [(v, exp)] body))] body2) =
  tocir (MLet [(v, exp)] body) ++ [Assign (CVar var) (head (tocir body2))]

tocir (MPlus (AVar a) (AVar b)) =
  [CPlus (CVar a) (CVar b)]

tocir (AExp (AInt b)) =
  [CInt b]

tocir (AExp (AVar b)) =
  [CVar b]
