module Ejercicio_e where

-- Dada una lista de enteros, implementar una función para devolver tuplas formadas por 
-- los elementos (sin repetir) de la lista, junto con la primera posición en la que aparecen. 
-- > primeraAparicion [1,5,6,0,2,6,4,78,9,41,-9,8,-9,12,45,0]  
-- [(1,1),(5,2),(6,3),(0,4),(2,5),(4,7),(78,8),(9,9),(41,10),
-- (-9,11),(8,12),(12,14),(45,15)]

-- lista original
-- posición
-- array auxiliar
-- salida
pAux :: [Int] -> Int -> [Int] -> [(Int,Int)]
pAux [] _ _ = []
pAux (x:xs) n ys
			| existe ys n = pAux xs (n+1) ys
			| otherwise = (x,n):pAux xs (n+1) (x:ys)


-- lista de enteros
-- entero a buscar 
existe :: [Int] -> Int -> Bool
existe [] _ = False
existe (x:xs) num
			| x == num = True
			| otherwise = existe xs num
