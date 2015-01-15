module Ejercicio_h where

{--
Teniendo en cuenta la definición de la función qs del apartado b) de este listado de 
ejercicios, se pide ordenar una lista de fechas mediante quicksort. Ejemplos de aplicación 
de la función serían: 

> qs [(Fecha 10 10 2013), (Fecha 24 12 2012), (Fecha 10 09 2013), (Fecha 
12 12 2013)]
 [24/12/2012,10/9/2013,10/10/2013,12/12/2013]
-}

data Fecha = Fecha Int Int Int 

instance Eq Fecha where (Fecha x y z) == (Fecha r s t) = x == r && y == s && z == t 
 
mismaFecha :: Fecha -> Fecha -> Bool 
mismaFecha f1 f2 = f1 == f2


-- Es necesario incluir todos los posibles casos (>, >=, <, <=), porque de lo 
-- contrario no sería capaz de aplicar algunos casos concretos y podría dar 
-- excepciones como stackoverflow al intentar ordenar una lista de fechas con qs 
instance Ord Fecha
	where
		(Fecha d1 m1 a1) < (Fecha d2 m2 a2) = d1 < d2 && m1 == m2 && a1 == a2 
				|| m1 < m2 && a1 == a2 || a1 < a2
 	 	(Fecha d1 m1 a1) > (Fecha d2 m2 a2) = d1 > d2 && m1 == m2 && a1 == a2
 	 			|| m1 > m2 && a1 == a2 || a1 > a2
 	 	(Fecha d1 m1 a1) <=(Fecha d2 m2 a2) = (Fecha d1 m1 a1) < (Fecha d2 m2 a2)
 	 			|| (Fecha d1 m1 a1) == (Fecha d2 m2 a2)
 		(Fecha d1 m1 a1) >= (Fecha d2 m2 a2) = (Fecha d1 m1 a1) > (Fecha d2 m2 a2)
 				|| (Fecha d1 m1 a1) == (Fecha d2 m2 a2) 
