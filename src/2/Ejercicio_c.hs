module Ejercicio_c where

-- Sumar los dobles de los elementos de una lista

-- Con recursividad no final
sumaDoble :: [Int] -> Int

sumaDoble []	= 0
sumaDoble [a]	= 2*a
sumaDoble (x:xs)= 2*x + sumaDoble xs

-- Con recursividad final o de cola
sumaDoblesRFinal :: [Int] ->Int
sumaDoblesRFinal lista = sumaDoblesAux lista 0

sumaDoblesAux :: [Int] -> Int -> Int
sumaDoblesAux [] result = result
sumaDoblesAux (x:xs) result = sumaDoblesAux xs (result + 2*x)


-- Con funciones anónimas y orden superior
sumaLista :: [Int] ->Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs
sumaDoblesOS :: [Int] ->Int
sumaDoblesOS l = sumaLista (map(\x ->x+x) l)
