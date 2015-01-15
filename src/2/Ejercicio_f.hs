module Ejercicio_f where

{--
Implementar  en  Haskell  una  función  que  calcule  el  número  de
secuencias de ceros que hay en una lista de números.

> ceros [0] > ceros[0,0]
1 				1

> ceros [0,1,0] > ceros [0,0,1,5,0,4,0,0,0,5]
2  					3
-}
ceros :: [Int] -> Int
ceros a = cuentaCeros a False 0

-- Lista de elementos
-- Anterior Es cero S/N
-- Contador de elementos 
cuentaCeros :: [Int] -> Bool -> Int -> Int

cuentaCeros [] _ a = a
cuentaCeros (0:xs) False a = cuentaCeros (xs) True (a+1)
cuentaCeros (0:xs) True a = cuentaCeros (xs) True a
cuentaCeros (_:xs) _ a = cuentaCeros (xs) False a
