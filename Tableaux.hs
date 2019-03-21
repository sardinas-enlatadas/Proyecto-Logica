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
{-
  --Busca si en la lista que le pasan existe una formula estilo p ^ -p
  --Si la encuentra devuelve True, si no, False
  hayContradiccion :: [Prop]-> [Prop] -> Bool
  hayContradiccion _ [] = False
  hayContradiccion [] _ = False
  hayContradiccion (a:as) l = if elem (Neg a) l then True else (hayContradiccion as l)
  --Metodo principal. Es el que llamas a la hora de buscar si la lista de proposiciones es satisfacible ,
  --Pasa la lista de props a fnn para que puedan ejecutarse las reglas alfa y beta.
  tableaux :: Prop -> Bool
  tableaux [] = True
  tableaux (x:xs) = tableaux1(map (fnn) (x:xs))
  --Recibe una lista de proposiciones y devuelve si existe un conjunto de estados tal que sea satisfacible
  --Primero busca una contradiccion entre las variables libres dentro del conjunto. Si la encuentra, devuelve False
  --Si no, manda a llamar a tableauxAux
  tableaux1 :: [Prop] -> Bool
  tableaux1 [] = True
  tableaux1 (x:xs)
    | hayContradiccion (x:xs) (x:xs) = False
    | otherwise = tableauxAux (ordena (x:xs)) (ordena (x:xs))
  --Recibe dos listas. La primera es sobre la que va a trabajar, la segunda es la lista completa de proposiciones originales
  --Si se da el caso de que se reciba un Var p, manda a llamar a cierraRamas con la lista de proposiciones originales
  --Si no es una variable, se manda a llamar recursivamente hasta que encuentre una Var
  --Hay dos casos recursivos. Que haya una disyuncion o una conjuncion
  -- Si hay una conjuncion busca que ambas ramas no se cierren
  -- Si hay una disyuncion busca que por lo menos una se mantenga abierta
  tableauxAux :: [Prop]-> [Prop] -> Bool
  tableauxAux _ [] =True
  tableauxAux [] _ = True
  tableauxAux ((Var p):xs) l = not (cierraRamas (Var p) l) && tableauxAux xs l
  tableauxAux ((Neg p):xs) l = cierraRamas (Neg p) l && tableauxAux xs l
  tableauxAux ((Conj x@(Var a) y@(Var b)):xs) l = cierraRamas x l && cierraRamas y l && tableauxAux xs l
  tableauxAux ((Conj x@(Var a) b):xs) l = (cierraRamas x l) && (tableauxAux f[b] l) && (tableauxAux xs l)
  tableauxAux ((Conj a x@(Var b)):xs) l = tableauxAux [a] l && cierraRamas x l && tableauxAux xs l
  tableauxAux ((Conj a b):xs) l = tableauxAux [a] l && tableauxAux [b] l && tableauxAux xs l
  tableauxAux ((Disy x@(Var a) y@(Var b)):xs) l = cierraRamas x l || cierraRamas y l && tableauxAux xs l
  tableauxAux ((Disy a x@(Var b) ):xs) l = tableauxAux [a] l || cierraRamas x l && tableauxAux xs  l
  tableauxAux ((Disy x@(Var a) b):xs) l = cierraRamas x l || tableauxAux [b] l && tableauxAux xs l
  tableauxAux ((Disy a b):xs) l= tableauxAux [a] l || tableauxAux [b] l && tableauxAux xs l
  --Pone primero las variables, las conjunciones y al final las disyunciones
  ordena:: [Prop] -> [Prop]
  ordena [] = []
  ordena ((Var p):xs )= (Var p):(ordena xs)
  ordena ((Neg a):xs )= (Neg a):(ordena xs)
  ordena ((Conj a b):xs )= (Conj a b):(ordena xs)
  ordena ((Disy a b):xs) = (ordena xs)++[(Disy a b)]

  --Devuelve True si la rama se cerro

  cierraRamas :: Prop -> [Prop] -> Bool
  cierraRamas x l = (elem (Neg x) l)
  data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)
  leaf x = Branch x Empty Empty
-}
--Manda a llamar a tableauxAux, que es el que trabaja con la formula en fnn
tableaux :: Prop -> Bool
tableaux a = tableauxAux (fnn a)
--Nos dice si la formula es satisfacible
tableauxAux :: Prop -> Bool
tableauxAux a = True
ordena :: Prop -> Tree Prop
ordena (Neg a) = leaf (Neg a)
ordena (TTrue) = leaf TTrue
ordena (FFalse) = leaf FFalse
ordena (Var p) = leaf (Var p)
ordena (Conj a b) = Branch (Conj a b) (Empty) (Branch a (Empty) (leaf b) )
ordena (Disy a b) = Branch (Disy a b) (leaf a) (leaf b)
