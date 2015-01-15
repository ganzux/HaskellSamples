module Ejercicio_k where

-- Implementa una función polimórfica en Haskell que reciba  2 listas y vaya cogiendo un 
-- elemento de la primera y dos de la segunda, creando una lista final de ternas. En caso 
-- de que una de las dos listas se acabe, mostrará la lista de ternas construidas hasta ese 
-- momento.
-- > mezclarEnTernas [4,5,8,90] [0,5,6,-9,8,-1,9,52,22]
-- [(4,0,5),(5,6,-9),(8,8,-1),(90,9,52)]
-- > mezclarEnTernas [1,2,3] [5,6,7,8]
-- [(1,5,6),(2,7,8)]
-- > mezclarEnTernas [1,2,3,4,5] "atropellado"
-- [(1,'a','t'),(2,'r','o'),(3,'p','e'),(4,'l','l'),(5,'a','d')]
-- > mezclarEnTernas [True,False] [2.3,5.9,5.7]
-- [(True,2.3,5.9)]

mezclarEnTernas :: [a] -> [b] -> [(a,b,b)]
mezclarEnTernas xs ys = mezclar2 xs ys []

mezclar2 :: [a] -> [b] -> [(a,b,b)]-> [(a,b,b)]
mezclar2 _ [] ys = ys	-- Vacío en el 2º
mezclar2 [] _ ys = ys	-- Vacío el 1º
mezclar2 _ [a] ys = ys	-- Sólo 1 elemento en el 2º
mezclar2 (x:xs) (z:zs:zss) ys = (x,z,zs):mezclar2 xs zss ys
