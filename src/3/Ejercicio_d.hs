module Ejercicio_d where

{--
Definir  un  tipo  moneda  para  representar  euros  y  dólares  USA.  Definir  una  función  que 
convierta entre ambas monedas sabiendo que el factor de conversión de euros a dólares 
es 1.14. 
-}

data Tipo = E | D deriving Show
data Moneda = M(Float, Tipo) deriving Show

convert :: Moneda -> Moneda
convert (M(x, E)) = M(x*1.14, D)
convert (M(x, D)) = M(x/1.14, E)




data Moneda2 = Euro(Float) | Dollar(Float) deriving Show
-- convert1 (Euro(1))
-- convert1 (Dollar(1))
convert1 :: Moneda2 ->Moneda2
convert1 (Euro(t)) = Dollar(t*1.14) 
convert1 (Dollar(t)) = Euro(t/1.14)
