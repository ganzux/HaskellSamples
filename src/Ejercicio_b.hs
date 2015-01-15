module Ejercicio_b where

{--
Se quiere ordenar los elementos de una lista (cuyos elementos son comparables) mediante 
el algoritmo del quicksort. 

-}
qs::Ord a => [a] -> [a] 
qs [] 	  = []
qs (p:xs) = qs [x|x<-xs,x<p] ++ [p] ++ qs [x|x<-xs,x>=p] 
