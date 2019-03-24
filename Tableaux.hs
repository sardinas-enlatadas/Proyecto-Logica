module  Tableaux where

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
-- Tipo de dato para representar expresiones de la lógica proposicional

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)
leaf x = Branch x Empty Empty
--El dato Crumb nos guarda el nodo del que nos movimos (papa) y el arbol que no pudimos visitar
-- Por ejemplo, el LeftCrumb lo que guarda el nodo padre y (Tree a) es el arbol derecho que no visitamos
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
--Tanto BreadCrumbs como Zipper son azucar sintactico
--BreadCrumbs es para guardar un camino hasta la raiz
type BreadCrumbs a = [Crumb a]
-- Zipper es una tupla, la primera entrada es el arbol actual y la segunda
-- es todo lo que hay arriba del nodo actual
type Zipper a= (Tree a, BreadCrumbs a)
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

zipperiza :: Prop -> Zipper Prop
zipperiza (Var p) = (leaf (Var p), [])
zipperiza (TTrue) = (leaf (TTrue),[])
zipperiza (FFalse) = (leaf (FFalse),[])
zipperiza (Neg a) = (leaf (Neg a),[])
zipperiza x@(Conj a b) = alfaReglaZipp x
zipperiza x@(Disy a b) = betaReglaZipp x

--Mete las conjunciones a un arbol
alfaRegla :: Prop -> Tree Prop
alfaRegla (Conj a (Var p)) = Branch (Conj a (Var p)) Empty (Branch (Var p) Empty (arboliza a))
alfaRegla (Conj (Var p) a) = Branch (Conj (Var p) a) Empty (Branch (Var p) Empty (arboliza a))
alfaRegla (Conj a b) = Branch (Conj a b) Empty (sumados)
  where sumados = suma (arboliza a) (arboliza b)

alfaReglaZipp :: Prop -> Zipper Prop
alfaReglaZipp (Conj a (Var p)) = (Branch (Conj a (Var p)) Empty (Branch (Var p) Empty (arboliza a)),[])
alfaReglaZipp (Conj (Var p) a) = (Branch (Conj (Var p) a) Empty (Branch (Var p) Empty (arboliza a)),[])
alfaReglaZipp (Conj a b) =( Branch (Conj a b) Empty (sumados),[])
  where sumados = suma (arboliza a) (arboliza b)
--Le agrega un arbol completo en la rama derecha a un arbol.
--llamar a esta funcion es la que mete conjunciones
suma :: Tree Prop -> Tree Prop ->Tree Prop
suma (Branch x Empty Empty) y = Branch x Empty y
suma (Branch a Empty t2) y = Branch a Empty (suma t2 y)
suma (Branch a t1 t2) y = Branch a (suma t1 y) (suma t2 y)
--La funcion que mete las disyunciones
betaRegla ::Prop -> Tree Prop
betaRegla (Disy a b) = Branch (Disy a b) (arboliza a) (arboliza b)

betaReglaZipp :: Prop -> Zipper Prop
betaReglaZipp (Disy a b) = ((Branch (Disy a b) (arboliza a) (arboliza b)),[])


sacarRamas :: Zipper Prop -> [Zipper Prop]
sacarRamas (Empty,z) = []
sacarRamas ( y@(Branch x Empty Empty),z) = [(y, z)]
sacarRamas (((Branch a t1 t2),zs)) = (sacarRamas (t1,(RightCrumb a t2):zs) )  ++ (sacarRamas (t2, ((LeftCrumb a t1):zs) ) )

moverseArriba :: Zipper a -> Zipper a
moverseArriba (t, LeftCrumb x r:bs) = (Branch x t r, bs)
moverseArriba (t, RightCrumb x l:bs) = (Branch x l t, bs)

sacaElemActual :: Zipper a ->  a
sacaElemActual ((Branch x _ _),_) = x
{-
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type BreadCrumbs a = [Crumb a]
type Zipper a= (Tree a, BreadCrumbs a)
-}
--Es una extension a la alfaRegla. La idea es que al ir agregando cada prop revise si su negacion ya pertenece al arbol
--Aun es necesario meter la proposicion a revisar a mano en la consola para probarla
sigueAbierta :: Zipper Prop-> Bool
sigueAbierta (_,[]) = True
sigueAbierta y@((Branch a Empty Empty), (x:xs)) =
  if a == (Neg (sacaElemActual (moverseArriba (y) ) ) ) then False
  else sigueAbierta ((Branch a Empty Empty), xs)
satisfacible :: Prop -> Bool
satisfacible a = elem True (map (sigueAbierta ) ( sacarRamas(zipperiza(fnn a)) ))
