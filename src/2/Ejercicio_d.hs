module Ejercicio_d where

import Ejercicio_c

-- Función que suma todos los elementos de una lista de enteros

suma :: [Int] -> Int
suma [] = 0
suma [a]= a
suma (x:xs) = x + suma(xs)




--- Versión usando las funciones de orden superior map filter
sumaCuadradosPares :: [Int] ->Int
sumaCuadradosPares lista = sumaLista (map(^2) (filter even lista))
--- Versión utilizando definición de funciones por comprensión
sumaCuadradosPares' :: [Int] ->Int
sumaCuadradosPares' lista = sumaLista [x^2 | x <- lista, even x]


