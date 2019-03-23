Estos son algunos comados que he estado usando para probar las funciones

ghci> arboliza (Disy (Conj (Disy (Var "P") (Var "Q") ) (Var "T") ) (Conj (Neg (Var "S")) (Var "L") ) )
--arboliza esta teniendo un problema ahora que lo veo
ghci> alfaReglaExtendida (Var "P") (arboliza (Conj (Neg (Var "P")) (Var "T")))
