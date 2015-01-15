module Ejercicio_g where

{--
Implementar una función en Haskell que reciba una lista de números
enteros y devuelva dos listas: una con los elementos sin repetir y otra
con  los  elementos  que  están  repetidos.

> repeticiones [0,6,0,8,-2,-5,4,-2,6,98,71,2,0,5]
([8,-5,4,98,71,2,5],[0,6,-2])
-}

repeticiones :: [Int] -> ([Int],[Int])
repeticiones a = repeticiones2 a [] []

-- Lista Original
-- Lista Repetidos
-- Lista Sin Repetir
-- Lista Destino
repeticiones2 :: [Int] -> [Int] -> [Int] -> ([Int],[Int])

repeticiones2 [] y z = (y,z)

repeticiones2 (x:xs) y z
-- Nos aseguramos de que no haya sido tratado ya
	| not (estaEnLista y x) && not (estaEnLista z x) =
		-- Si es repetido, a la lista de repetidos
		if estaEnLista xs x then repeticiones2 xs (x:y) z
		-- No es repetido, a la lista de los no repetidos
			else repeticiones2 xs y (x:z)
-- Si ya ha sidfo tratado, llamamos al siguiente elemento
	| otherwise = repeticiones2 xs y z



estaEnLista :: [Int] -> Int -> Bool
estaEnLista [] _ = False
estaEnLista (x:xs) a = if a == x then True else estaEnLista xs a




repeticionesAux :: [Int] -> [Int] -> [Int] -> ([Int],[Int])

repeticionesAux [] repes noRepes = (repes, noRepes)

repeticionesAux (x:xs) repes noRepes
  | not(existe repes x) && existe xs x  = repeticionesAux xs (x:repes) noRepes
  | not(existe repes x) && not(existe xs x)=repeticionesAux xs repes (x:noRepes)
  | otherwise = repeticionesAux xs repes noRepes

existe:: [Int] -> Int -> Bool
existe [] _ = False
existe (x:xs) n = if n==x then True else existe xs n
