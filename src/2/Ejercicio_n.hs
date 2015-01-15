module Ejercicio_n where

{--
Define una función polimórfica que sea capaz de invertir los elementos
de una lista. Se piden diferentes versiones:
- Con recursividad no final
- Con recursividad de cola o final
- Utilizando la función de orden superior foldr

> reverse'2 [1,2,3]
[3,2,1]

> reverse'2 "casa"
"asac"
-}

-- recursividad no final
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--recursividad final
reverse'2 :: [a] ->[a]
reverse'2 lista = reverseAux lista []

reverseAux :: [a] ->[a] ->[a]
reverseAux [] result = result
reverseAux (x:xs) result = reverseAux xs (result++[x])


invertir :: [a] ->[a]
invertir = foldr(\x xs ->xs ++ [x]) []
