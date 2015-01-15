module Ejercicio_p where

{--
Implementar  la  función  predefinida  de  la  librería  estándar  flip.  Esta
función lo que hace es recibir una función y devolver otra función que
es  idéntica  a  la  función  original,  salvo  que  intercambia  los  dos
primeros parámetros. 

> flip' zip [1,2,3] "casa"
[('c',1),('a',2),('s',3)]

> flip' (+) 3 4
7

> flip' (++) "casa" "pollo"
"pollocasa"
-}

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x
