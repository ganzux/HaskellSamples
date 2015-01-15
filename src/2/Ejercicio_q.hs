module Ejercicio_q where

{--
Implementar la función polimórfica predefinida de la librería estándar
map.  Esta  función  lo  que  hace  es  recibir  una  función  y  una  lista  y
devuelve la lista resultante de aplicar la función a cada elemento de la
lista original. 

> map (3*) [1,2,3]
[3,6,9]

> map doble [1,2,3]
[2,4,6]

> map not [True,False]
[False,True]
-}

map'' :: (a -> b) -> [a] -> [b]
map'' _ [] = []
map'' f (x:xs) = f x : map'' f xs
