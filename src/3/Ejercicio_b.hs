module Ejercicio_b where

{--
Se pide varias funciones para hacer lo siguiente: 
1.  Función  que  dado  un  punto  de  coordenadas  y  una  dirección  (Norte,  Sur,  Este  u 
Oeste) mueva el punto hacia la dirección indicada. Un ejemplo de aplicación de la 
función sería: 

> mover Este (3,4)    > mover Norte (3.5,9.2) 
 (			4,4)      (				3.5,10.2) 
-}

data Direccion  = N | S | E | O  -- Norte, Sur, Este, Oeste
type Coordenada = (Float, Float) -- x , y

mover :: Direccion -> Coordenada -> Coordenada
mover N (x,y) = (x,y+1)
mover S (x,y) = (x,y-1)
mover E (x,y) = (x+1,y)
mover O (x,y) = (x-1,y)

{--
2.  Función  que  dados  dos  puntos  de  coordenadas  indique cuál  está  más  al  sur. 
Ejemplos de aplicación de la función son: 
> masAlSur (3,5) (4,6)   > masAlSur (4.5,-6.2) (4.5,-7) 
(3.0,5.0)     (4.5,-7.0)
-} 

masAlSur :: Coordenada -> Coordenada -> Coordenada
masAlSur (x1,y1) (x2,y2) = if y1 <= y2 then (x1,y1) else( x2,y2)

{--
3.  Función que calcule la distancia entre dos puntos: 
-}

distancia :: Coordenada -> Coordenada -> Coordenada 
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2 +(y1-y2)^2)

{--
4.  Función  que  dado  un  punto  y  una  lista  de  direcciones,  retorne  el  camino  que 
forman  todos  los  puntos  después  de  cada  movimiento  sucesivo  desde  el  punto 
original: 
-}

camino :: Coordenada -> [Direccion] -> [Coordenada]
camino _ [] = []
camino xy (x:xs) = (mover x xy) : camino (mover x xy) xs 







