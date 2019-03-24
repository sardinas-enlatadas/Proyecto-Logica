module  Tableaux2 where
import Data.List
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
    show (Conj p q) ="(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
    show (Disy p q) ="(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)

{-FlexibleInstances Show [Prop] where
    show [TTrue] = "[ True ]" -- T
    show [FFalse] = "[ False ]" -- F
    show [(Var x)] = "[ "++ x ++" ]" -- P
    show [(Neg p)] = "[¬ " ++ show p ++" ]" -- ¬ P
    show [(Conj p q)] = "[(" ++ show p ++ " ∧ " ++ show q ++ ")]" -- (P ∧ Q)
    show [(Disy p q)] = "[(" ++ show p ++ " ∨ " ++ show q ++ ")]" -- (P ∨ Q)
    show [(Impl p q)] = "[(" ++ show p ++ " → " ++ show q ++ ")]" -- (P → Q)
    show [(Syss p q)] = "[(" ++ show p ++ " ↔ " ++ show q ++ ")]" -- (P ↔ Q)
-}

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
{-
enlistaProp:: Prop-> [Prop]
enlistaProp (TTrue) =[TTrue]
enlistaProp (FFalse)= [FFalse]
enlistaProp (Var p)= [Var p]
enlistaProp (Neg(Var p)) = [Neg(Var p)]
enlistaProp (Conj a b) = [(Conj a b)]++enlistaProp a ++ enlistaProp b
enlistaProp (Disy a b) = [(Disy a b)]++enlistaProp a ++ enlistaProp b
-}
listaAtom :: Prop -> [[Prop]]
listaAtom (TTrue) =[[TTrue]]
listaAtom (FFalse)= [[FFalse]]
listaAtom (Var p)= [[Var p]]
listaAtom (Neg(Var p)) = [[Neg(Var p)]]
listaAtom (Conj a b)= listaAtom a ++ listaAtom b
listaAtom (Disy a b)=listaAtom a ++ listaAtom b

empaqueta:: Prop-> [[Prop]]->[[Prop]]
empaqueta (TTrue) a = map (++[TTrue]) a
empaqueta (FFalse) a= map (++[FFalse]) a
empaqueta (Var p) a= map (++[(Var p)]) a
empaqueta (Neg(Var p)) a = map (++[(Neg(Var p))]) a
--empaqueta (Disy (Var x) (Var y)) a = (map (++([(Var x)]++[(Var y)])) a)--(concat(a,(empaqueta x ))) `union`(concat( a, empaqueta y a))
empaqueta (Conj (x) (y)) a = (map (++([x]++[y])) a)
empaqueta (Disy x y) a = empaqueta x a ++ empaqueta y a
--empaqueta (Disy x y) a = union[(empaqueta x a),empaqueta y a]
--empaqueta (Disy (Var x)(Var y)) =
--empaqueta (Conj (x) (y)) a = empaqueta x a `union` empaqueta (y) a
--map (map (++(empaqueta y a)) a) (map (++(empaqueta x a)) a)
