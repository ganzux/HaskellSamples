module Ejercicio_c where

{--

La  empresa  RealTimeSolutions,  Inc.  está  trabajando  en  un  controlador  para  una  central 
domótica.  El  controlador  recibe  información  de  termostatos  situados  en  diferentes 
habitaciones  de  la  vivienda  y  basándose  en  esta  información,  activa  o  desactiva  el  aire 
acondicionado  en  cada  una  de  las  habitaciones.  Los  termostatos  pueden  enviar  la 
información sobre la temperatura en grados Celsius o Fahrenheit. A su vez, los aparatos de 
aire  acondicionado  reciben  dos  tipos  de  órdenes:  apagar  y  encender  (on  y  off). Se  pide: 

1.  Definir  un  tipo  de  datos  para  representar  las  temperaturas  en  ambos  tipos  de 
unidades. 

2.  Definir  una  función  convert  que  dada  una  temperatura  en  grados  Celsius  la 
convierta a grados Fahrenheit y viceversa. (Conversión de C a F: f = c * 9/5 + 32; 
conversión de F a C: c = (f – 32) * 5/9.)  

3.  Definir  un  tipo  de  datos  para  representar  las  órdenes  a  los  aparatos  de  a/a. 

4.  Definir  una  función  action  que  dada  una  temperatura en  cierta  habitación 
determine  la  acción  a  realizar  sobre  el  aparato  de  a/a  de  dicha  habitación.  El 
controlador debe encender el aparato si la temperatura excede de 28ºC. Ejemplos 
de aplicación:  

> action(Celsius(25))   > action(Fahrenheit(83.5)) 
On      Off 
-}

data Tipo = F | C
data Temp = T(Float,Tipo)
data Orden = On | Off deriving Show

convert :: Temp -> Temp
convert (T(x, F)) = T((x-32)*(5/9), C)	-- F -> C
convert (T(x, C)) = T((x*(9/5))+32, F)	-- C -> F

action :: Temp -> Orden
action (T(x, C)) = if x <= 28 then On else Off
action x = action (convert x) 
