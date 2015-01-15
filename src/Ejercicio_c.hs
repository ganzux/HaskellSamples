module Ejercicio_c where

{--
Se pide implementar una función que dada un número (de cualquier tipo que soporte la 
operación de división) y una lista de números del mismo tipo, divida a ese número por cada 
uno de los elementos contenidos en la lista y devuelva una lista con el resultado. 

Ejemplos de aplicación de la función son: 

> divisiones 5 [1,2,3] 
[Just 5,Just 2,Just 1] 

> divisiones 5 [1,2,3,0,9,10] 
[Just 5,Just 2,Just 1,Nothing,Just 0,Just 0]
-}

division' :: Integral a => a -> a -> Maybe a 
division' _ 0 = Nothing 
division' m n = Just (m `div` n) 

divisiones :: Integral a => a -> [a] -> [Maybe a] 
divisiones _ [] = [] 
divisiones numerador (denominador:xs) = if (divisionR == Nothing)
	then (Nothing:divisiones numerador xs) 
 	else (divisionR:divisiones numerador xs) 
	where divisionR = (division' numerador denominador)
