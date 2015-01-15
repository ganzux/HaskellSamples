module Ejercicio_o where

import Ejercicio_n

{--
Define una función polimórfica que sea capaz de invertir los elementos
de una lista de listas.
> reverse'' [[1,2,3],[3,4,5]]
[[5,4,3],[3,2,1]]

> reverse'' ["pepe", "casa", "patio"]
["oitap","asac","epep"]
-}

reverse'' :: [[a]] -> [[a]]
reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [reverse' x]
