module Ejercicio_f where

{--
Dado el siguiente tipo de datos que representa un árbol binario: 

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show 

Se pide definir una función que calcule el espejo de un árbol. 

Ejemplos de aplicación de la función serían: 

> espejo (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV)) 
Rama (Rama AV 4 AV) 5 (Rama AV 8 (Rama AV 60 AV)) 

> espejo (Rama AV 5 (Rama AV 4 AV)) 
Rama (Rama AV 4 AV) 5 AV 
-}

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show 

espejo :: Arbol a ->Arbol a 
espejo AV = AV 
espejo (Rama AV r AV) = Rama AV r AV 
espejo (Rama i r d) = (Rama ed r ei) 
	where ei = espejo i; ed = espejo d
