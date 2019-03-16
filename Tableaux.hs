module  Tableaux where

  -- Tipo de dato para representar expresiones de la lógica proposicional
data Prop = TTrue
          | FFalse
          | Var String
          | Neg Prop
          | Conj Prop Prop -- (P ∧ Q)
          | Disy Prop Prop -- (P ∨ Q)
          | Impl Prop Prop -- (P → Q)
          | Syss Prop Prop -- (P ↔ Q)
  				deriving (Eq,Ord)

  -- show de Prop
instance Show Prop where
    show TTrue = "True" -- T
    show FFalse = "False" -- F
    show (Var x) = x -- P
    show (Neg p) = "¬ " ++ show p -- ¬ P
    show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
    show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)

pasarAFNC:: Prop -> Prop
pasarAFNC a = fnc(fnn a)

fnc :: Prop -> Prop
fnc TTrue = TTrue
fnc FFalse = FFalse
fnc (Var p) = Var p
fnc (Neg (Var a)) = Neg(Var a)
fnc z@(Conj (Var p) (Var q))= z
fnc z@(Conj (Var p) (Not q)) = z
fnc z@(Conj (Not q) (Var p)) = fnc (Conj (Var p) (Not q)) 

fnc (Conj (Disy a b) (Var q) ) = Conj (fnc (Disy a b)) (Var q)
fnc (Conj (Var p) (Disy a b)) = fnc (Conj (Disy a b) (Var p))
fnc (Conj x@(Disy a b) y@(Disy c d)) = Conj (fnc x) (fnc y)
fnn :: Prop -> Prop
fnn TTrue = TTrue
fnn FFalse = FFalse
fnn (Var p) = (Var p)
fnn z@(Neg (Var p)) = z
fnn (Neg (Neg (Var p))) = Var p
fnn (Neg (Neg (q))) = fnn q
fnn (Neg (Impl (p) (q))) = (Conj (fnn p) (Neg (fnn q)))
fnn (Neg (Syss (p) (q))) = fnn (Neg (Conj (Impl p q ) (Impl q p)))
fnn (Neg (Conj p q)) = Disy ( fnn (Neg p)) (fnn (Neg q))
fnn (Neg (Disy p q)) = Conj (fnn (Neg p)) (fnn (Neg q))
fnn (Conj p q) = Conj (fnn p) (fnn q)
fnn (Disy p q) = Disy (fnn p) (fnn q)
fnn (Impl p q) = Disy (fnn (Neg p)) (fnn q)
fnn (Syss p q) = Conj (fnn (Impl p q)) (fnn (Impl q p))
