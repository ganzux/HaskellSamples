module Ejercicio_l where

{--
Se  pide  una  función  polimórfica  en  Haskell  que  dado  un  elemento  y
una lista añada dicho elemento al final de la lista.

> alFinal 3 [1,2,6,7]
[1,2,6,7,3]

> alFinal True [False,False]
[False,False,True]

> alFinal 'k' "casita"
"casitak"
-}


alFinal :: a -> [a] -> [a]
alFinal a	[]		= [a]
alFinal a	(x:xn)	= [x] ++ alFinal a xn
