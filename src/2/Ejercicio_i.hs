module Ejercicio_i where

{--
Implementa  una  función  incluye  en  Haskell  que  reciba  dos  listas  de
números enteros y nos diga si la primera de las listas está contenida
en  la  segunda.  Se  dice  que  una  lista  está  contenida  en  otra  si  los
elementos de la primera aparecen dentro de la segunda, en el mismo
orden y de forma consecutiva. 
> incluye [] [4,5]   > incluye [4,4,2] [5,4,4,5,4,4,2,9]
			True  				True 
> incluye [4,4,2] [5,4,4,5,2,9] > incluye [4,5] [] 
			False 				False
-}

incluye:: [Int] -> [Int] -> Bool
incluye lista1 lista2 = incluyeAux lista1 lista2 lista1 [] False

incluyeAux:: [Int] -> [Int] -> [Int] -> [Int] -> Bool -> Bool

-- Si hemos avanzado hasta el final de la subcadena que buscábamos: fin
incluyeAux [] _ _ _ _ = True

-- Si por más que hemos buscado, hemos llegado al final de la lista dentro de la cual
-- estábamos buscando la subcadena: fin
incluyeAux _ [] _ _ _ = False

incluyeAux (x:xs) (y:ys) listaXAux listaYAux b 
    -- Los elementos son iguales y ya estábamos reconociendo la subcadena
    -- -> avanzamos sin tocar las dos listas auxiliares 
  | x==y && b = incluyeAux xs ys listaXAux listaYAux True
    -- Los elementos son iguales, y no estábamos reconociendo al subcadena
    -- (los elementos anteriores diferían). Tenemos que guardar el valor actual de ys,
    -- para poder volver a este punto si al final resulta que no encontramos toda
    -- la subcadena (p.ej. incluyeAux 0:1:[] 0:0:1:[],
    -- guardamos 0:1:[] por si necesitamos reiniciar la búsqueda).
  | x==y && not b = incluyeAux xs ys (x:xs) ys True
    -- Los elementos no concuerda, pero estábamos reconociendo la subcadena.
    -- Abortamos y reiniciamos desde el siguiente elemento donde empezó la concordancia.
    -- Además tenemos que reiniciar también la subcadena a buscar
  | x /= y && b = incluyeAux listaXAux listaYAux listaXAux [] False
    -- Los elementos no concuerdan y tampoco estábamos reconociendo la cadena.
    -- Seguimos buscando con la subcadena inicial (listaXAux). 
  | x /= y && not b = incluyeAux listaXAux ys listaXAux [] False
