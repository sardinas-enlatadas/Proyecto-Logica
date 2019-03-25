module  Tableaux where
--Tipo de dato utilizado para representar las proposiciones
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
--Ya que un Tableaux solo recibe formulas en fnn fue necesario importar esta funcion
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
fnn (Conj (Var p) (Var q)) = if (Var p) ==(Var q) then (Var p) else (Conj (Var p) (Var q))
fnn (Conj p q) = Conj (fnn p) (fnn q)
fnn (Disy p q) = Disy (fnn p) (fnn q)
fnn (Impl p q) = Disy (fnn (Neg p)) (fnn q)
fnn (Syss p q) = Conj (fnn (Impl p q)) (fnn (Impl q p))

-- Tipo de dato para representar expresiones de la lógica proposicional como arboles

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Ord, Eq)
leaf x = Branch x Empty Empty
--El dato Crumb nos guarda el nodo del que nos movimos, el arbol que no pudimos visitar y nos indica como nos movimos,
--hacia la izquierda LeftCrumb, o hacia la derecha RightCrumb
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
--Extension de Arboliza. La idea de zipperiza es poder pasar una proposicion a un tipo Zipper para conocer
--Mas acerca de su padre. Zipper tambien nos deja conocer el arbol "hermano" (al mismo "nivel"), sin embargo
--Para motivos de esta practica decidimos dejar al hermano como Empty, para no guardar cosas que no necsitamos
zipperiza :: Prop -> Zipper Prop
zipperiza (Var p) = (leaf (Var p), [])
zipperiza (TTrue) = (leaf (TTrue),[])
zipperiza (FFalse) = (leaf (FFalse),[])
zipperiza (Neg a) = (leaf (Neg a),[])
zipperiza x@(Conj a b) = alfaReglaZipp x
zipperiza x@(Disy a b) = betaReglaZipp x

--Recibe una proposicion y la regresa en forma de arbol.
--Mete las conjunciones a un arbol
--De nuevo, deja la proposicion original como nodo principal, Empty como hijo izquierdo y el desglose
--De la conjuncion como hijo derecho.
alfaRegla :: Prop -> Tree Prop
alfaRegla (Conj a (Var p)) = Branch (Conj a (Var p)) Empty (Branch (Var p) Empty (arboliza a))
alfaRegla (Conj (Var p) a) = Branch (Conj (Var p) a) Empty (Branch (Var p) Empty (arboliza a))
alfaRegla (Conj a b) = Branch (Conj a b) Empty (sumados)
  where sumados = suma (arboliza a) (arboliza b)

--Recibe dos arboles y al primer arbol le agrega en sus hojas el arbol derecho.
--Ocupamos esta funcion para que la alfaRegla pueda acceder tanto a su hijo proximo
--Tanto como al hijo del hijo
suma :: Tree Prop -> Tree Prop ->Tree Prop
suma (Branch x Empty Empty) y = Branch x Empty y
suma (Branch a Empty t2) y = Branch a Empty (suma t2 y)
suma (Branch a t1 t2) y = Branch a (suma t1 y) (suma t2 y)

--Recibe una proposicion y la regresa en su forma Zipper
--Extension de alfaRegla para que podamos ocuparlo en Zippers
alfaReglaZipp :: Prop -> Zipper Prop
alfaReglaZipp (Conj a (Var p)) = (Branch (Conj a (Var p)) Empty (Branch (Var p) Empty (arboliza a)),[])
alfaReglaZipp (Conj (Var p) a) = (Branch (Conj (Var p) a) Empty (Branch (Var p) Empty (arboliza a)),[])
alfaReglaZipp (Conj a b) =( Branch (Conj a b) Empty (sumados),[])
  where sumados = suma (arboliza a) (arboliza b)

--Recibe una disyuncion y regresa un arbol con la disyuncion como nodo principal
--La primer parte de la disyuncion como hijo izquierdo y la segunda como hijo derecho
betaRegla ::Prop -> Tree Prop
betaRegla (Disy a b) = Branch (Disy a b) (arboliza a) (arboliza b)

--Extension de la betaRegla para acoplarla al modelo de Zippers
betaReglaZipp :: Prop -> Zipper Prop
betaReglaZipp (Disy a b) = ((Branch (Disy a b) (arboliza a) (arboliza b)),[])

--Recibe un Zipper Prop y regresa una lista que contiene todas las hojas que hay desde ese punto y para abajo.
--Esta funcion es de las mas importantes pues es con estas hojas son con las que vamos a trabajar.
--Ya que son Zipper Prop tambien guarda todo el recorrido desde la proposicion actual Hasta la Raiz
--Pusimos LeftCrumb a Empty y RightCrumb a Empty, pues no nos importa el arbol "hermano"
sacarRamas :: Zipper Prop -> [Zipper Prop]
sacarRamas (Empty,z) = []
sacarRamas ( y@(Branch x Empty Empty),z) = [(y, z)]
sacarRamas (((Branch a t1 t2),zs)) = (sacarRamas (t1,(LeftCrumb a Empty):zs) )  ++ (sacarRamas (t2, ((RightCrumb a Empty):zs) ) )

--Recibe un Zipper y regresa el Zipper "subido" un nivel
--Si se movio a la izquierda, entonces subimos un nivel y ponemos al arbol actual como hijo izquierdo
--De la misma forma si se movio a la derecha, subimos un nivel y ponemos el arbol actual como hijo derecho
moverseArriba :: Zipper a -> Zipper a
moverseArriba (t, LeftCrumb x r:bs) = (Branch x t r, bs)
moverseArriba (t, RightCrumb x l:bs) = (Branch x l t, bs)

--Saca la proposicion del arbol en el que nos encontramos actualmente
--Ponemos el caso Empty,x pues si no cuando saquemos el sacaElemActual de un Empty la cosa truena.
--Decidimos poner un FFalse para que no afecte de ninguna forma
sacaElemActual :: Zipper Prop ->  Prop
sacaElemActual (Empty,x) = FFalse
sacaElemActual ((Branch x _ _),_) = x

--Sirve para lo mismo que sacaElemActual, solo que este recibe un Tree, no un Zipp
sacaElemActualT :: Tree Prop -> Prop
sacaElemActualT Empty = FFalse
sacaElemActualT (Branch a _ _ )= a

--Le pasamos un Zipper Prop y regresamos una lista de todos las proposiciones que estan en sus nodos padre;
--Incluimos al nodo actual en la lista
enlistaPadres :: Zipper Prop -> [Prop]
enlistaPadres (_,[]) = []
enlistaPadres (t, ((RightCrumb x l):xs) ) = (sacaElemActualT t):x:(enlistaPadres (Empty, xs))
enlistaPadres (t, ((LeftCrumb x l):xs)) = (sacaElemActualT t):x:(enlistaPadres (Empty, xs))
--Revisamos si en una lista de prop esta su negacion.
--Esta funcion se manda a llamar con cada hoja del arbol. Devuelve True si no encontramos una contradiccion entre
--el camino del nodo actual Hasta la Raiz
--Manda a llamar a sigueAbierta2Aux que es la que realiza la comparacion.
sigueAbierta2:: [Prop]->Bool
sigueAbierta2 [] = True
sigueAbierta2 xs = sigueAbierta2Aux xs xs
--Esta funcion es la que realmente hace el trabajo de comprobar que no haya contradicciones en la lista
--Recibe dos listas iguales, la primera que es para ir revisando elemento por elemento si su negacion esta en la segunda lista
sigueAbierta2Aux :: [Prop]-> [Prop] -> Bool
sigueAbierta2Aux [] _ = True
sigueAbierta2Aux (x:xs) [] = True
sigueAbierta2Aux (x:xs) z = if elem (fnn(Neg x)) z then False else sigueAbierta2Aux xs z
--Recibe una proposicion y devuelve True si hay un modelo que satisfaga I(a)=1, False en caso de que no sea satisfacible
--Primero pasa la proposicion a fnn
--Luego la vuelve un Zipper Prop
--Le saca las ramas al Zipper Prop
--A cada rama del Zipper le aplica la funcion enlistaPadres para tener una lista
--De listas. Una lista de listas en la que cada elemento representa todo el camino desde una hoja hasta la raiz
--Le aplica sigueAbierta2 a cada camino de la hoja hasta la raiz, es decir pasa una lista de Bool en la que cada
--Elemento es la representacion de si la rama quedo abierta o no
--Al final verifica si quedo algun True (una rama abierta) en la lista
satisfacible2 :: Prop -> Bool
satisfacible2 a = elem True (map (sigueAbierta2) (map (enlistaPadres) (sacarRamas(zipperiza (fnn (fnn a ))) ) ))

--Recibe una prop y devuelve True si es tautologia o False en caso de que no
--Para saber si es tautologia, Simplemente negamos la proposicion y le aplicamos satisfacible2
--Si satisfacible regresa True, quiere decir la negacion no implica que todas las ramas se cierren, por eso lo negamos
--De igual manera, si satisfacible regresa False, entonces no hay interpretacion que deje ramas abiertas, por lo Tanto
--Si es tautologia; por eso solo le metimos el not a satisfacible
tautologia2 :: Prop -> Bool
tautologia2 a = not (satisfacible2 (fnn (fnn(Neg a))))
