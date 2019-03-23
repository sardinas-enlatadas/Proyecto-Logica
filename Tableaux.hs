module  Tableaux where

  -- Tipo de dato para representar expresiones de la lógica proposicional

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)
leaf x = Branch x Empty Empty

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
--Pasa una proposicion a su forma de arbol. Supone que ya esta en fnn
--Las conjunciones las mete en el arbol derecho, las disyunciones las divide en dos ramas.
--Guarda la proposicion original como el nodo padre.
arboliza :: Prop -> Tree Prop
arboliza (Var p) = leaf (Var p)
arboliza (TTrue) = leaf (TTrue)
arboliza (FFalse) = leaf (FFalse)
arboliza (Neg a) = leaf (Neg a)
arboliza x@(Conj a b) = alfaRegla x
arboliza x@(Disy a b) = betaRegla x
--Mete las conjunciones a un arbol
alfaRegla :: Prop -> Tree Prop
alfaRegla (Conj a b) = Branch (Conj a b) Empty (sumados)
  where sumados = suma (sacaDer(arboliza a)) (arboliza b)

--Es una extension a la alfaRegla. La idea es que al ir agregando cada prop revise si su negacion ya pertenece al arbol
--Aun es necesario meter la proposicion a revisar a mano en la consola para probarla
alfaReglaExtendida :: Prop -> Tree Prop-> Bool
alfaReglaExtendida _ Empty = True
alfaReglaExtendida a (Branch b Empty Empty) = if (Neg a) == b then False else True
alfaReglaExtendida a (Branch x t1 t2) =
    if esta (Neg(a))  (Branch x t1 t2) then False else True
--Es la funcion que sirve para buscar si una proposicion ya esta dentro del arbol
esta :: Prop -> Tree Prop -> Bool
esta x Empty = False
esta x (Branch a Empty Empty) = x == a
esta x (Branch a t1 t2) = if x == a || esta x t1  || esta x t2 then True else False
--Nos regresa el elemento mas a la derecha del arbol
sacaDer :: Tree Prop ->Tree Prop
sacaDer y@(Branch x Empty Empty) = y
sacaDer (Branch x _ t2) = sacaDer t2
--Le agrega un arbol completo en la rama derecha a un arbol. No fue necesario poner mas casos pues la unica funcion que manda a
--llamar a esta funcion es la que mete conjunciones
suma :: Tree Prop -> Tree Prop ->Tree Prop
suma (Branch x Empty Empty) y = Branch x Empty y
--La funcion que mete las disyunciones
betaRegla ::Prop -> Tree Prop
betaRegla (Disy a b) = Branch (Disy a b) (arboliza a) (arboliza b)
