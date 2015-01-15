module Ejercicio_h where

{--
Dada  una  lista  de  números  enteros  implementar  una  función  que
devuelva una lista con los nelementos mayores de la lista original. 

> nmayores [8,4,-5,6,-1,0,2,6,-10,7] 4
[8,7,6,6]

> nmayores [8,4,-5,6,-1,0,2,6,-10,7] 7
[8,4,6,6,7,0,2]

> nmayores [8,4,-5,6,-1,0,2,6,-10,7] 11
[8,4,-5,6,-1,0,2,6,-10,7]
-}
nmayores :: [Int] -> Int -> [Int]
nmayores x num = nmayoresAux x num 0 []


-- Lista original
-- Número de elementos a almacenar
-- Número de elementos que llevamos
-- Lista con los mayores
nmayoresAux :: [Int] -> Int -> Int -> [Int] -> [Int]
nmayoresAux [] _ _ listaDestino = listaDestino
nmayoresAux (x:xn) elementos tamano listaDestino
	| tamano == elementos	= nmayoresAux xn elementos tamano     (reemplazaMenor listaDestino x [])
	| otherwise 			= nmayoresAux xn elementos (tamano+1) (x:listaDestino)
	

-- Lista de Origen
-- Elemento a Comparar
-- Lista Destino
reemplazaMenor :: [Int] -> Int -> [Int] -> [Int]
reemplazaMenor [] _ listaDestino = listaDestino
reemplazaMenor (x:xs) num listaDestino
	| x < menor	= reemplazaMenor xs num (num:listaDestino)
	| otherwise	= reemplazaMenor xs num (x:listaDestino)
			where menor = menor xs
	
-- Devuelve el menor de una lista
menor :: [Int] ->Int
menor [] = 0
menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs
