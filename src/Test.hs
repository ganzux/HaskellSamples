module Test where

data Tipo = F | C | K

data Temp = T(Float,Tipo)

data Orden = On | Off deriving Show


convert :: Temp -> Temp

convert (T(x, F)) = T((x-32)*(5/9), C)	-- F -> C

convert (T(x, C)) = T((x*(9/5))+32, F)	-- C -> F




action :: Temp -> Orden

action (T(x, C)) = if x <= 28 then On else Off

action x = action (convert x) 
