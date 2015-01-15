module Ejercicio_i where

{--
Se pide crear una nueva clase de tipos, llamada Coleccion, para representar colecciones de 
datos de cualquier tipo, donde los tipos pertenecientes a esta clase tendrán el siguiente 
comportamiento: 
esVacia: función para saber si la colección está vacía. 
insertar: insertará un nuevo elemento en la colección. 
 primero: devolverá el primer elemento de la colección. 
 eliminar: eliminará un elemento de la colección. 
 size: devolverá el número de elementos de la colección.
Algunas de las funciones anteriores variarán su implementación en función del tipo de 
colección particular que sea instancia de la clase Coleccion. Por ello, se pide crear dos 
instancias diferentes de esta clase para los dos nuevos tipos de datos que se presentan a 
continuación: 
 data Pila a = Pil [a] deriving Show 
 data Cola a = Col [a] deriving Show 
El primero de ellos representa una estructura de datos LIFO con elementos de tipo a. El 
segundo representa una estructura de datos FIFO de elementos de tipo a. 
Ejemplos de aplicación de las funciones para ambos tipos de datos serían: 
> insertar 10 (Col [1,2,3,4]) 
Col [1,2,3,4,10] 
> insertar 10 (Pil [1,2,3,4]) 
Pil [1,2,3,4,10] 
> primero (Col [1,2,3,4,10]) 
1 
> primero (Pil [1,2,3,4,10]) 
10 
> eliminar (Col [1,2,3,4,10]) 
Col [2,3,4,10] 
> eliminar (Pil [1,2,3,4,10]) 
Pil [1,2,3,4] 

-}
-- Clase que representa una colección de elementos
-- Es necesario acompañarle de dos cosas, una que representa la colección en sí
-- y otra el tipo de elementos que contiene (variable a que representa cualquier tipo)
class Coleccion c where
 	esVacia :: c a -> Bool 
 	insertar :: a -> c a -> c a 
 	primero :: c a -> a 
 	eliminar :: c a -> c a 
	size :: c a -> Int 

data Pila a = Pil [a] deriving Show 

instance Coleccion Pila where
	esVacia (Pil p) = length p == 0 
 	insertar e (Pil p) = Pil (p ++ [e]) 
 	-- last devuelve el último elemento de la lista
 	primero (Pil p) = last p 
 	eliminar (Pil p) = Pil (init p) 
 	size (Pil l) = length l 
 
data Cola a = Col [a] deriving Show 
instance Coleccion Cola where
 esVacia (Col c) = length c == 0 
 -- en una cola se insertan los elementos por el final 
 insertar e (Col c) = Col (c ++ [e]) 
 -- en una cola se sacan los elementos por el principio
 primero (Col c) = head c 
 -- se elimina el primero que entró en la cola
 eliminar (Col (c:cs)) = Col cs 
 size (Col c) = length c 
