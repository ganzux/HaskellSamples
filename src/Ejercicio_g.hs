module Ejercicio_g where

{--
Teniendo en cuenta el nuevo tipo de datos Fecha definido anteriormente, se pide una 
función que sea capaz de comparar dos fechas. Ejemplos de aplicación de la función serían: 

> mismaFecha (Fecha 10 10 2013) (Fecha 10 10 2013) 
True 
> mismaFecha (Fecha 10 11 2013) (Fecha 10 10 2013) 
False 
-}
data Fecha = Fecha Int Int Int 

instance Eq Fecha where (Fecha x y z) == (Fecha r s t) = x == r && y == s && z == t 
 
mismaFecha :: Fecha -> Fecha -> Bool 
mismaFecha f1 f2 = f1 == f2
