import Prelude hiding (drop)

-- Algorithmic Types
data Typ =
    TInt
  | TVar String
  | EVar String
  | Forall String Typ
  | FType Typ Typ
  deriving (Show, Eq)

-- Terms
data Exp =
    Var String
  | Lit Int
  | Lam String Exp
  | App Exp Exp
  | Ann Exp Typ
  deriving (Show, Eq)

-- Algorithmic Chain
data Chain =
    Sub Typ Typ
  | Chk Exp Typ
  | Inf Exp (Typ -> Chain)
  | AInf Typ Exp (Typ -> Chain)

-- Worklist
data Worklist =
    Empty
  | ConsTVar Worklist String
  | ConsEVar Worklist String
  | ConsBind Worklist String Typ
  | ConsJug Worklist Chain

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
tsubst s t (FType t1 t2) =
  FType (tsubst s t t1) (tsubst s t t2)

csubst :: String -> Typ -> Chain -> Chain
csubst s t (Sub a b) = Sub (tsubst s t a) (tsubst s t b)
csubst s t (Chk a b) = Chk (esubst s t a) (tsubst s t b)
csubst s t (Inf e f) = Inf (esubst s t e) (\t1 -> csubst s t (f t1))
csubst s t (AInf t1 e f) = AInf (tsubst s t t1) (esubst s t e) (\t1 -> csubst s t (f t1))

wsubst :: String -> Typ -> Worklist -> Worklist
wsubst = error "TODO"

replace :: String -> String -> String -> Worklist -> Worklist
replace a a1 a2 Empty           = Empty
replace a a1 a2 (ConsTVar l s)  = ConsTVar (replace a a1 a2 l) s 
replace a a1 a2 (ConsEVar l s)
  | s == a     = ConsEVar (ConsEVar l a1) a2
  | otherwise  = ConsEVar (replace a a1 a2 l) s
replace a a1 a2 (ConsBind l s t)  =
  ConsBind (replace a a1 a2 l) s t -- (tsubst a (FType (TVar a1) (TVar a2)) t)
replace a a1 a2 (ConsJug l c)  =
  ConsJug (replace a a1 a2 l) c -- (csubst a (FType (TVar a1) (TVar a2)) c) 

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

prec :: Worklist -> String -> String -> Bool
prec = error "TODO"

step :: Worklist -> Worklist
-- First 3 rules
step (ConsTVar l a) = l
step (ConsEVar l a) = l
step (ConsBind l x t) = l
-- Rules 4 to 6 and 12 and 13
step (ConsJug l (Sub TInt TInt)) = l
step (ConsJug l (Sub (TVar a) (TVar b)))
   | a == b    = l
   | otherwise = error "1"
step (ConsJug l (Sub (EVar a) (EVar b))) 
   | a == b     = l
   | prec l a b = wsubst b (EVar a) (drop b l) -- rule 12
   | otherwise  = wsubst a (EVar b) (drop a l) -- rule 13
-- Rules 14 to 17
step (ConsJug l (Sub (TVar a) (EVar b)))
   | prec l a b = wsubst b (TVar a) (drop b l)
   | otherwise  = error "2"
step (ConsJug l (Sub (TVar a) (EVar b)))
   | prec l a b = wsubst b (TVar a) (drop b l)
   | otherwise  = error "3"
step (ConsJug l (Sub TInt (EVar b))) = wsubst b TInt (drop b l)
step (ConsJug l (Sub (EVar b) TInt)) = wsubst b TInt (drop b l)
-- rule 7
step (ConsJug l (Sub (FType a b) (FType c d))) = ConsJug (ConsJug l (Sub b d)) (Sub c a) 
-- rules 9 and 8
step (ConsJug l (Sub t1 (Forall a t2))) = ConsJug (ConsTVar l a) (Sub t1 t2)   
step (ConsJug l (Sub (Forall a t1) t2)) = ConsJug (ConsEVar l a) (Sub (tsubst a (EVar a) t1) t2)   
-- rules 10 and 11
step (ConsJug l (Sub (EVar a) (FType b c))) = error "TODO"
step (ConsJug l (Sub (FType b c) (EVar a))) = error "TODO"
