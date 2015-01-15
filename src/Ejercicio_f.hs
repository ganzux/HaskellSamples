module Ejercicio_f where

{--
Se quiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, para ello se 
deberá crear un nuevo tipo de datos en Haskell. Por ejemplo, si se crea un nuevo tipo de 
datos cuyo constructor de datos es Fecha, en el intérprete al poner fechas concretas nos 
devolvería la representación de la fecha que hayamos definido: 
> Fecha 10 10 2013 > Fecha 24 12 2012 
10/10/2013 				24/12/2012 

-}
data Fecha = Fecha Int Int Int 
 
instance Show Fecha where show (Fecha x y z) = show x ++ "/" ++ show y ++ "/" ++ show z 
