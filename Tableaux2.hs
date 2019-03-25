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

listaAtom :: Prop -> [[Prop]]
listaAtom (TTrue) =[[TTrue]]
listaAtom (FFalse)= [[FFalse]]
listaAtom (Var p)= [[Var p]]
listaAtom (Neg(Var p)) = [[Neg(Var p)]]
listaAtom (Conj a b)= listaAtom a ++ listaAtom b
listaAtom (Disy a b)=listaAtom a ++ listaAtom b

concatListas:: [[a]] ->[a]
concatListas =concatMap (take 2)

empaqueta:: Prop-> [[Prop]]->[[Prop]]
empaqueta (TTrue) a = map (`union`[TTrue]) a
empaqueta (FFalse) a= map (`union`[FFalse]) a
empaqueta (Var p) a= map (`union`[(Var p)]) a
empaqueta (Neg(Var p)) a = map (`union`[(Neg(Var p))]) a
empaqueta (Disy x y) a = empaqueta x a ++ empaqueta y a
empaqueta (Conj x y) a = (map (`union`(concatListas(empaqueta x a)`union`concatListas(empaqueta y a))) a)

tauto :: Prop -> [[Prop]]
tauto p= (empaqueta (fnn(fnn(Neg(p)))) (listaAtom(fnn(fnn(Neg(p))))))

buscaNegado:: [Prop] -> Bool
buscaNegado [] = False
buscaNegado (x:xs)= if elem (fnn(Neg(x))) (xs)
  then True
  else buscaNegado (xs)
--Devuelve True si todas las ramas estan cerradas
tautologia :: Prop -> [Bool]
--tautologia []=[]
tautologia l= map (buscaNegado) (tauto l)

tautologia1::[Bool]->Bool
tautologia1 []=True
tautologia1 (x:xs)= x && tautologia1 (xs)
