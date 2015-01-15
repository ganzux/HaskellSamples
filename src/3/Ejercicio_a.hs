module Ejercicio_a where

{--

Se pide una función que dada una lista de racionales, donde cada racional se define como 
dos  números  enteros  (numerador  y  denominador),  y  un número  racional,  devuelva  otra 
lista con todos los racionales equivalentes al dado. Realiza dos versiones del ejercicio: 
1.  Empleando type.
2.  Empleando data. 

Ejemplos de aplicación (si se utiliza type) serían:  
> equivalentes [(2,4),(3,5),(4,8)] (1,2) 
[(2.0,4.0),(4.0,8.0)]
> equivalents [(3,5)] (1,2) 
 [] 

Ejemplos de aplicación (si se utiliza data) serían:  
> equivalentes[R(2,4),R(3,5),R(4,8)] (R(1,2)) 
[R (2.0,4.0),R (4.0,8.0)]
> equivalentes [R(3,5)] (R(1,2)) 
[] 
-}

-- TYPE
type Racional = (Float, Float)

-- Lista original
-- Dato a comprobar
equivalents :: [Racional] -> Racional -> [Racional]
equivalents [] _ = []
equivalents (x:xs) r = if esEquivalente x r
						then [x] ++ equivalents xs r
						else equivalents xs r

esEquivalente :: Racional -> Racional -> Bool
esEquivalente (x1,y1) (x2,y2) = (x1/y1 == x2/y2) 

-- DATA
data Racional1 = R(Float,Float) deriving Show
equivalentes :: [Racional1] -> Racional1 -> [Racional1] 
equivalentes [] _ = [] 
equivalentes (x:xs) r = if isEquivalent x r
						then [x] ++ equivalentes xs r
						else equivalentes xs r

isEquivalent :: Racional1 -> Racional1 -> Bool 
isEquivalent (R(x1,y1)) (R(x2,y2)) = (x1/y1 == x2/y2) 
