module Desugar where

import Parser

desugar :: Exp -> Exp
desugar (And a b) =
  (If a b (Bool False))

desugar (Or a b) =
  (If a (Bool True) b)

desugar (Eq e e2) =
  Eq (desugar e) (desugar e2)

desugar (Not e) =
  Not (desugar e)

desugar (Let bindings body) =
  Let (desugarBindings bindings) (desugar body)
  where
    desugarBindings :: [(Exp, Exp)] -> [(Exp, Exp)]
    desugarBindings [] = []
    desugarBindings ((var, exp):xs) =
      (var, desugar exp) : desugarBindings xs
    
desugar (If cnd thn els) =
  If (desugar cnd) (desugar thn) (desugar els)

desugar (Set var exp) =
  Set var (desugar exp)

desugar (Begin exps) =
  Begin (desugarBegin exps)
  where
    desugarBegin :: [Exp] -> [Exp]
    desugarBegin [] = []
    desugarBegin ((e:xs)) =
      desugar e : desugarBegin xs

desugar (While cnd exp) =
  While (desugar cnd) (desugar exp)

desugar e =
  e
