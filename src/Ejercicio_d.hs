module Ejercicio_d where

{--
Dado un nuevo tipo de datos para representar un árbol binario de cualquier tipo, definido 
como sigue: 
 data Arbol a = AV | Rama (Arbol a) a (Arbol a) 
Se pide definir una función que visualice el árbol por pantalla de una determinada forma: 
separando cada hijo izquierdo y derecho por “|”, la raíz entre guiones y cada nivel diferente 
del árbol por “( )”. Ejemplos de aplicación de la función sería los siguientes:

> mostrarArbol (Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV)) 
"((60)|-8-|())|-5-|(4)" 

> mostrarArbol (Rama AV 5 (Rama AV 4 AV)) 
"()|-5-|(4)" 

¿Sería equivalente a declarar el nuevo tipo de datos Arbol como una instancia de la clase Show? 
data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show
-}

data Arbol a = AV | Rama (Arbol a) a (Arbol a) deriving Show 

mostrarArbol :: (Show a) => Arbol a -> String 
mostrarArbol (Hoja x) = show x 
mostrarArbol (Rama izqdo der) = "(" ++ mostrarArbol izqdo ++ "|" ++ mostrarArbol der ++ ")" 

--Rama (Rama (Rama AV 60 AV) 8 AV) 5 (Rama AV 4 AV) 
