module Ejercicio_m where

{--
Mediante la programación de orden superior se pide implementar una
de  las  funciones  predefinidas  en  la  librería  estándar  de  Haskell:  la
función  zipWith.  Esta  función  recibe  como  parámetros  una  función  y
dos  listas  y  une  ambas  listas  aplicado  la  función  entre  los
correspondientes parámetros. 

> zipWith' max [6,3,2,1] [7,3,1,5]
[7,3,2,5]

> zipWith' (++) ["hola ", "ciao ", "hi "] ["pepe", "ciao", "peter"]
["hola pepe","ciao ciao","hi peter"]
ghci> zipWith' (*) (replicate 5 2) [1..]
[2,4,6,8,10]

> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6]] [[3,2,2],[3,4,5]]
[[3,4,6],[9,20,30]]

> zipWith’ crearTupla [1,2,3] "casita"
[(1,'c'),(2,'a'),(3,'s')]


(Suponiendo que la función crearTupla tiene la siguiente definición:
crearTupla :: a-> b-> (a,b)
crearTupla x y = (x,y)
)
-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
