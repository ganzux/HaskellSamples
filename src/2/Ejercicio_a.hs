module Ejercicio_a where

{--
Implementa una función en Haskell que elimine de una lista de enteros
aquellos números múltiplo de x.

> cribar [0,5,8,9,-9,6,0,85,-12,15] 2
[5,9,-9,85,15]

Se piden diferentes versiones de la misma función:
	- Con definición de listas por comprensión
	- Con recursividad no final
	- Con recursividad final o de cola
-}
notMultiplo :: Int ->Int ->Bool
notMultiplo x y = x `mod` y /= 0

-- (con recursividad no final)
cribarRNFinal :: [Int] -> Int -> [Int]
cribarRNFinal [] n = []
cribarRNFinal (x:xs) n = if(notMultiplo x n)
		then x: cribarRNFinal xs n
		else cribarRNFinal xs n
			
-- (con definición de listas por compresión))
cribar :: [Int] ->Int ->[Int]
cribar lista n = [x|x <- lista, notMultiplo x n]


-- (con recursividad final o de cola)
cribarRFinal :: [Int] ->Int ->[Int]
cribarRFinal lista n = cribarAux lista n []
cribarAux :: [Int] ->Int ->[Int] ->[Int] 
cribarAux [] n r = r
cribarAux (x:xs) n listaR = if(notMultiplo x n)
	then cribarAux xs n (listaR++[x])
	else cribarAux xs n listaR
