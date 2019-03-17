
pasarAFNC:: Prop -> Prop
pasarAFNC a = fnc(fnn a)

fnc :: Prop -> Prop
fnc TTrue = TTrue
fnc FFalse = FFalse
fnc (Var p) = Var p

fnc z@(Neg (Var a)) = z
fnc z@(Conj (Var p) (Var q))= z
fnc z@(Conj (Var p) (Neg q)) = z
fnc (Conj (Neg q) (Var p)) = fnc (Conj (Var p) (Neg q))

fnc (Conj (Disy a b) (Var q) ) = Conj (fnc (Disy a b)) (Var q)
fnc (Conj (Var p) (Disy a b)) = fnc (Conj (Disy a b) (Var p))
fnc (Conj x@(Disy a b) y@(Disy c d)) = Conj (fnc x) (fnc y)

fnc (Conj (Conj a b) (Var p)) = Conj (pasarAFNC (Disy (Neg a) (Neg b) ) ) (Var p)
fnc (Conj (Var p) (Conj a b)) = fnc (Conj (Conj a b) (Var p))
fnc (Conj x@(Conj a b) y@(Conj c d)) = Conj (pasarAFNC (Neg x )) (pasarAFNC (Neg y))

fnc (Disy (Var p) (Var q)) = Conj (Neg (Var p) ) (Neg (Var q))
fnc (Disy (Neg p) (Var q)) = Conj p (Var q)
fnc (Disy (Var p) (Neg q)) = fnc (Disy (Neg q) (Var p))

fnc (Disy (Conj a b) (Var p) )= Conj (fnc (Disy a (Var p))) (fnc (Disy b (Var p)))
fnc (Disy (Var p) (Conj a b)) = fnc (Disy (Conj a b) (Var p))
fnc (Disy (Conj a b) (Conj c d)) = Conj (pasarAFNC (Disy (Neg a ) (Neg b)) )  (pasarAFNC (Disy (Neg c) (Neg d) ))

fnc x@(Disy (Disy a b) (Var p)) =  pasarAFNC (Neg x)
fnc (Disy (Var p) (Disy a b)) = fnc (Disy (Disy a b) (Var p))
fnc x@(Disy (Disy a b) (Disy c d)) = pasarAFNC (Neg x)
--fnc (Disy (Disy a b) (Var p) ) =

  -- (pvq) v (t)
  -- (-p ^ -q)  ^ -t
