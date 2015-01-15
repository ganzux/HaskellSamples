module Ejercicio_j where

{--
Dada  una  lista  de  enteros,  se  pide  implementar  una  función  que
ordene  dicha  lista  de  menor  a  mayor  utilizando  un  algoritmo  de
inserción. Dicho algoritmo de inserción consiste en recorrer la lista L,
insertando cada elemento L[i] en el lugar correcto entre los elementos
ya ordenados L[1] ,...,L[i-1].
> ordenar [2,3,1]
[1,2,3]
> ordenar [1,0,4,0,6,9]
[0,0,1,4,6,9]
-}

ordenar :: [Int] -> [Int]
ordenar []		= []
ordenar [x] 	= [x]
ordenar (x:xs)	= insertarOrdenado x (ordenar xs)

insertarOrdenado:: Int ->[Int]->[Int]
insertarOrdenado x [] = [x]
insertarOrdenado x (l:ls) = if x < l then(x:l:ls) else l:insertarOrdenado x ls
