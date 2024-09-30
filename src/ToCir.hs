module ToCir where

import Parser 
import ToAnf
import qualified Data.Map as Map

-- this module makes the ANF AST's order of execution explicit

data Goto = Goto String deriving Show

data Cir =
  CInt Int
  | CVar String
  | CBool Bool
  | CPlus Cir Cir
  | CMinus Cir Cir
  | CReturn Cir
  | CLess Cir Cir
  | CEq Cir Cir
  | CGreater Cir Cir
  | CNot Cir
  | IfStmt Cir Cir Cir
  | IfGoto Cir Goto Goto
  | Assign Cir Cir 
  | CBegin [Cir]
  | IfGotoLoop Cir Goto
  | CWhileLoop Cir Cir deriving Show

mk :: String -> ([Cir], Map.Map String [Cir])
mk exp =
  makeexplicit (toanf (parseExp (lexer exp)))

makeexplicit :: MonExp -> ([Cir], Map.Map String [Cir])
makeexplicit exp =
  -- makes the order of execution explicit
  toc (tocir' (tocir exp))
  
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
  [Assign (CVar var) (head (tocir exp))] ++ tocir body

tocir (MPlus (AVar a) (AVar b)) =
  [CPlus (CVar a) (CVar b)]

tocir (MPlus (AVar a) (AInt b)) =
  [CPlus (CVar a) (CInt b)]

tocir (MPlus (AInt a) (AVar b)) =
  [CPlus (CInt a) (CVar b)]

tocir (MPlus (AInt a) (AInt b)) =
  [CPlus (CInt a) (CInt b)]
  
tocir (MMinus (AVar a) (AVar b)) =
  [CMinus (CVar a) (CVar b)]

tocir (MMinus (AVar a) (AInt b)) =
  [CMinus (CVar a) (CInt b)]

tocir (MMinus (AInt a) (AVar b)) =
  [CMinus (CInt a) (CVar b)]

tocir (MMinus (AInt a) (AInt b)) =
  [CMinus (CInt a) (CInt b)]

tocir (MLess (AVar x) (AInt y)) =
  [CLess (CVar x) (CInt y)]

tocir (MLess (AInt a) (AVar b)) =
  [CLess (CInt a) (CVar b)]

tocir (MLess (AInt x) (AInt y)) =
  [CLess (CInt x) (CInt y)]

tocir (MLess (AVar a) (AVar b)) =
  [CLess (CVar a) (CVar b)]

tocir (MGreater (AVar a) (AVar b)) =
  [CGreater (CVar a) (CVar b)]

tocir (MGreater (AVar a) (AInt b)) =
  [CGreater (CVar a) (CInt b)]

tocir (MGreater (AInt a) (AVar b)) =
  [CGreater (CInt a) (CVar b)]

tocir (MGreater (AInt x) (AInt y)) =
  [CGreater (CInt x) (CInt y)]

tocir (MEq (AVar x) (ABool y)) =
  [CEq (CVar x) (CBool y)]

tocir (MEq (ABool x) (AVar y)) =
  [CEq (CBool x) (CVar y)]

tocir (MEq (AVar x) (AVar y)) =
  [CEq (CVar x) (CVar y)]
  
tocir (MEq (ABool x) (ABool y)) =
  [CEq (CBool x) (CBool y)]

tocir (MEq (AVar x) (AInt y)) =
  [CEq (CVar x) (CInt y)]

tocir (MEq (AInt x) (AVar y)) =
  [CEq (CInt x) (CVar y)]
  
tocir (MEq (AInt x) (AInt y)) =
  [CEq (CInt x) (CInt y)]
  
tocir (AExp (AInt b)) =
  [CInt b]

tocir (MIf (AExp (AVar tmp)) thn els) =
  [IfStmt (CVar tmp) (head (tocir thn)) (head (tocir els))]
  
tocir (AExp (AVar b)) =
  [CVar b]

tocir (MBegin exps) =
  [CBegin (mbeginToCir exps)]
  where
    mbeginToCir :: [MonExp] -> [Cir]
    mbeginToCir [] = []
    mbeginToCir (x:xs) =
      tocir x ++  mbeginToCir xs

tocir (MSetBang (AVar var) exp) =
  [(Assign (CVar var) (head (tocir exp)))]

tocir (MWhileLoop cnd body) =
  [CWhileLoop (head (tocir cnd)) (head (tocir body))]

tocir (MNot (AVar v)) =
  [CNot (CVar v)]
  
tocir (MNot (ABool b)) =
  [CNot (CBool b)]
  
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

toc :: [Cir] -> ([Cir], Map.Map String [Cir])
toc exps = toc' exps 0 Map.empty
  where
    toc' :: [Cir] -> Int -> Map.Map String [Cir] -> ([Cir], Map.Map String [Cir])
    toc' [] _ blocks = ([], blocks)
    
    toc' ((IfStmt cnd thn els):xs) counter blocks =
      let blockThn = "block_" ++ show counter
          blockEls = "block_" ++ show (counter + 1)
          gotoThn = Goto blockThn
          gotoEls = Goto blockEls
          blocks' = Map.insert blockThn [thn] blocks
          blocks'' = Map.insert blockEls [els] blocks'
          exp = IfGoto cnd (Goto blockThn) (Goto blockEls)
          (restExps, finalBlocks) = toc' xs (counter + 2) blocks''
      in (exp : restExps, finalBlocks)
    
    toc' ((Assign var (IfStmt cnd thn els)):xs) counter blocks =
      let blockThn = "block_" ++ show counter
          blockEls = "block_" ++ show (counter + 1)
          gotoThn = Goto blockThn
          gotoEls = Goto blockEls
          blocks' = Map.insert blockThn [thn] blocks
          blocks'' = Map.insert blockEls [els] blocks'
          exp = IfGoto cnd (Goto blockThn) (Goto blockEls)
          (restExps, finalBlocks) = toc' xs (counter + 2) blocks''
      in (exp : restExps, finalBlocks)

    toc' ((CBegin ((CWhileLoop cnd body):ys)):xs) counter blocks =
      let blockLoop = "loop_" ++ show counter
          blockBody = "block_" ++ show (counter + 1)
          blocks' = Map.insert blockLoop [cnd] blocks
          blocks'' = Map.insert blockLoop [body] blocks'
          exp = IfGotoLoop cnd (Goto blockLoop)
          (restExps, finalBlocks) = toc' xs (counter + 2) blocks''
          in (exp : ys ++ restExps, finalBlocks)
             
    toc' (x:xs) counter blocks = 
      let (restExps, finalBlocks) = toc' xs counter blocks
      in (x : restExps, finalBlocks)
