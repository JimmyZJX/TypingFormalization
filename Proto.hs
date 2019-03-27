{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

import Prelude hiding (drop)
import Data.List
import Data.Maybe

-- Algorithmic Types
data Typ =
    TInt
  | TVar String
  | EVar String
  | Forall String Typ
  | TArr Typ Typ
  deriving (Eq)

instance Show Typ where
  show TInt = "Int"
  show (TVar x) = x
  show (EVar x) = "^" ++ x
  show (Forall x t) = "(all " ++ x ++ ". " ++ show t ++ ")"
  show (TArr a b) = show a ++ " -> " ++ show b

-- Terms
data Exp =
    Var String
  | Lit Int
  | Lam String Exp
  | App Exp Exp
  | Ann Exp Typ
  deriving (Eq)

instance Show Exp where
  show (Var x) = x
  show (Lit n) = show n
  show (Lam x e) = "(\\" ++ x ++ " -> " ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Ann e t) = show e ++ " :: " ++ show t

-- Algorithmic Chain
data Chain =
    Sub Typ Typ
  | Chk Exp Typ
  | Inf Exp (Typ -> Chain)
  | AInf Typ Exp (Typ -> Chain)

instance Show Chain where
  show c = show' c 0
    where
      show' (Sub a b) _ = show a ++ " <: " ++ show b
      show' (Chk e t) _ = show e ++ " <= " ++ show t
      show' (Inf e c) n = show e ++ " =>" ++ show n ++ " " ++
                            show' (c (TVar $ show n)) (n + 1)
      show' (AInf a e c) n = show a ++ " * " ++ show e ++ " =>>" ++ show n ++ " " ++
                            show' (c (TVar $ show n)) (n + 1)

-- Worklist
type Worklist = [Work]
data Work =
    WTVar String
  | WEVar String
  | WBind String Typ
  | WJug Chain
  deriving Show

instance Show [Work] where
  show [] = "."
  show (WTVar x : w) = show w ++ ", " ++ x
  show (WEVar x : w) = show w ++ ", ^" ++ x
  show (WBind x t : w) = show w ++ ", " ++ x ++ " : " ++ show t
  show (WJug c : w) = show w ++ " ||- " ++ show c


esubst :: String -> Typ -> Exp -> Exp
esubst e s (Lam x b) = Lam x (esubst e s b)
esubst e s (App e1 e2) = App (esubst e s e1) (esubst e s e2)
esubst e s (Ann e1 t) = Ann (esubst e s e1) (tsubst e s t)
esubst e s t = t

tsubst :: String -> Typ -> Typ -> Typ
tsubst s t TInt = TInt
tsubst s t (TVar a) = TVar a
tsubst s t (EVar x)
  | s == x     = t
  | otherwise  = EVar x
tsubst s t (Forall a b)
  | s == a     = Forall a b
  | otherwise  = Forall a (tsubst s t b)
tsubst s t (TArr t1 t2) =
  TArr (tsubst s t t1) (tsubst s t t2)

ttsubst :: String -> Typ -> Typ -> Typ
ttsubst s t TInt = TInt
ttsubst s t (EVar a) = EVar a
ttsubst s t (TVar x)
  | s == x     = t
  | otherwise  = TVar x
ttsubst s t (Forall a b)
  | s == a     = Forall a b
  | otherwise  = Forall a (ttsubst s t b)
ttsubst s t (TArr t1 t2) =
  TArr (ttsubst s t t1) (ttsubst s t t2)

csubst :: String -> Typ -> Chain -> Chain
csubst s t (Sub a b) = Sub (tsubst s t a) (tsubst s t b)
csubst s t (Chk e a) = Chk (esubst s t e) (tsubst s t a)
csubst s t (Inf e f) = Inf (esubst s t e) (\t1 -> csubst s t (f t1))
csubst s t (AInf t1 e f) = AInf (tsubst s t t1) (esubst s t e) (\t1 -> csubst s t (f t1))

-- substitute a existential variable by its solution and replace its declaration by xs
wsubst :: String -> [String] -> Typ -> Worklist -> Worklist
wsubst s xs t = concatMap wsubst1
  where
    wsubst1 (WTVar x)   = [WTVar x]
    wsubst1 (WEVar x)
      | s == x    = reverse $ map WEVar xs
      | otherwise = [WEVar x]
    wsubst1 (WBind x a) = [WBind x (tsubst s t a)]
    wsubst1 (WJug c)    = [WJug (csubst s t c)]

ftv :: Typ -> [Typ]
ftv TInt = []
ftv (TVar a) = [TVar a]
ftv (EVar x) = [EVar x]
ftv (Forall a b) = delete (TVar a) (ftv b)
ftv (TArr t1 t2) = union (ftv t1) (ftv t2)

{-
wsubst _ _ _ Empty = Empty
wsubst s xs t (ConsTVar w x)   = ConsTVar (wsubst s xs t w) x
wsubst s xs t (ConsEVar w x)
  | s == x    = foldr (\x w -> ConsEVar w x) (wsubst s xs t w) xs
  | otherwise = ConsEVar (wsubst s xs t w) x
wsubst s xs t (ConsBind w x a) = ConsBind (wsubst s xs t w) x (tsubst s t a)
wsubst s xs t (ConsJug w c)    = ConsJug (wsubst s xs t w) (csubst s t c)

replace :: String -> String -> String -> Worklist -> Worklist
replace a a1 a2 Empty           = Empty
replace a a1 a2 (ConsTVar l s)  = ConsTVar (replace a a1 a2 l) s 
replace a a1 a2 (ConsEVar l s)
  | s == a     = ConsEVar (ConsEVar l a1) a2
  | otherwise  = ConsEVar (replace a a1 a2 l) s
replace a a1 a2 (ConsBind l s t)  =
  ConsBind (replace a a1 a2 l) s t -- (tsubst a (TArr (TVar a1) (TVar a2)) t)
replace a a1 a2 (ConsJug l c)  =
  ConsJug (replace a a1 a2 l) c -- (csubst a (TArr (TVar a1) (TVar a2)) c) 

drop :: String -> Worklist -> Worklist
drop s Empty            = Empty
drop s (ConsTVar w x)   
  | s == x              = w
  | otherwise           = ConsTVar (drop s w) x
drop s (ConsBind w x t) = (ConsBind (drop s w) x t)
drop s (ConsJug w c)    = ConsJug (drop s w) c
drop s (ConsEVar w x)
  | s == x              = w
  | otherwise           = ConsEVar (drop s w) x
--}

-- Is `^a` declared before `^b`?
prec :: Worklist -> String -> String -> Bool
prec w a b = elem a . dropWhile (/= b) $ wex
  where
    wex = concatMap (\x -> case x of
        WEVar v -> [v]
        _       -> []
      ) w


genSplit x = (x ++ "1", x ++ "2")

findBind :: String -> Worklist -> Maybe Typ
findBind x (WBind y a : w)
  | x == y    = Just a
  | otherwise = findBind x w
findBind x (_ : w) = findBind x w
findBind _ [] = Nothing

pickNewVar w = [fromJust $ find (\c -> all (\var -> c `notElem` var) wvars) ['a'..'z']]
  where
    wvars = concatMap (\x -> case x of
        WEVar v -> [v]
        WTVar v -> [v]
        _ -> []
      ) w

step :: Worklist -> Worklist
-- First 3 rules
step (WTVar a : w) = w
step (WEVar a : w) = w
step (WBind x t : w) = w

-- Subtyping --
-- Rules 4 to 6 and 12 and 13
step (WJug (Sub TInt TInt) : w) = w
step (WJug (Sub (TVar a) (TVar b)) : w)
  | a == b    = w
  | otherwise = error "1"
step (WJug (Sub (EVar a) (EVar b)) : w) 
  | a == b     = w
  | prec w a b = wsubst b [] (EVar a) w -- rule 12
  | otherwise  = wsubst a [] (EVar b) w -- rule 13
-- Rules 14 to 17
step (WJug (Sub (TVar a) (EVar b)) : w)
  | prec w a b = wsubst b [] (TVar a) w
  | otherwise  = error "2"
step (WJug (Sub (EVar b) (TVar a)) : w)
  | prec w a b = wsubst b [] (TVar a) w
  | otherwise  = error "3"
step (WJug (Sub TInt (EVar b)) : w) = wsubst b [] TInt w
step (WJug (Sub (EVar b) TInt) : w) = wsubst b [] TInt w
-- rule 7
step (WJug (Sub (TArr a b) (TArr c d)) : w) = WJug (Sub c a) : WJug (Sub b d) : w
-- rules 9 and 8
step (WJug (Sub t1 (Forall a t2)) : w) = WJug (Sub t1 (ttsubst a (TVar x) t2)) : WTVar x : w
    where x = pickNewVar w
step (WJug (Sub (Forall a t1) t2) : w) = WJug (Sub (ttsubst a (EVar x) t1) t2) : WEVar x : w
    where x = pickNewVar w
-- rules 10 and 11
step (WJug (Sub (EVar a) (TArr b c)) : w)
  | EVar a `notElem` ftv (TArr b c) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $ WJug (Sub (EVar a) (TArr b c)) : w
  | otherwise = error "10"
step (WJug (Sub (TArr b c) (EVar a)) : w)
  | EVar a `notElem` ftv (TArr b c) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $ WJug (Sub (TArr b c) (EVar a)) : w
  | otherwise = error "11"

-- Checking --
-- rules 19-21
step (WJug (Chk e (Forall a t)) : w) = WJug (Chk e (ttsubst a (TVar x) t)) : WTVar x : w
    where x = pickNewVar w
step (WJug (Chk (Lam x e) (TArr a b)) : w) = WJug (Chk e b) : WBind x a : w
step (WJug (Chk (Lam x e) (EVar a)) : w) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $
        WJug (Chk e (EVar a2)) : WBind x (EVar a1) : w
      -- TODO gen new var
-- rule 18
step (WJug (Chk e b) : w) = WJug (Inf e (\a -> Sub a b)) : w

-- Inference --
step (WJug (Inf (Var x) c) : w) = case findBind x w of
    Just a  -> WJug (c a) : w
    Nothing -> error $ "No binding for " ++ x
step (WJug (Inf (Ann e a) c) : w) = WJug (Chk e a) : WJug (c a) : w
step (WJug (Inf (Lit _) c) : w) = WJug (c TInt) : w
step (WJug (Inf (Lam x e) c) : w) = WJug (Chk e (EVar b)) : WBind x (EVar a) :
      WJug (c (TArr (EVar a) (EVar b))) : WEVar b : WEVar a : w
    where
      a = pickNewVar w
      b = pickNewVar (WEVar a : w)
      -- TODO gen new var
step (WJug (Inf (App e1 e2) c) : w) = WJug (Inf e1 (\b -> AInf b e2 c)) : w

-- Application Inference --
step (WJug (AInf (Forall a t) e c) : w) = WJug (AInf (ttsubst a (EVar x) t) e c) : WEVar x : w
    where x = pickNewVar w
step (WJug (AInf (TArr a b) e c) : w) = WJug (Chk e a) : WJug (c b) : w
step (WJug (AInf (EVar a) e c) : w) = let (a1, a2) = genSplit a
      in wsubst a [a1, a2] (TArr (EVar a1) (EVar a2)) $ WJug (AInf (EVar a) e c) : w

runStep :: Worklist -> IO ()
runStep [] = putStrLn "Done."
runStep w = do
  print w
  runStep (step w)

-- Sample worklists
ta = TVar "a"
tb = TVar "b"

success1 = [WJug (Sub (Forall "a" (TArr ta TInt)) (TArr (Forall "a" (TArr ta ta)) TInt))]

failure1 = [WJug (Sub (Forall "a" (TArr TInt ta)) (TArr TInt (Forall "b" tb)))]

typing1 = [WJug (Chk (App (Lam "x" (Var "x")) (Lit 33)) TInt)]
