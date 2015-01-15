module DocbleOMas where

doble :: Integer -> Integer
cuadruple :: Integer -> Integer

doble x = x + x
cuadruple x = doble( doble(x) )

divReal :: (Integer, Integer) -> Double
divReal (x, y) = fromInteger(x) / fromInteger(y) 
