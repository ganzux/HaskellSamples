module Ejercicio_a where

{--

a)  Definir una función que dado un día de la semana, indique si éste es o no laborable. Para 
representar el día de la semana se deberá crear un nuevo tipo enumerado. 

-}

data DiaSemanas = L|M|X|J|V|S|D deriving (Eq, Show, Ord)

isWeekEnd :: DiaSemanas -> Bool
isWeekEnd x = (x == S || x == D)


data DiaSemana = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo deriving (Show,Eq) 
 
laborables :: [DiaSemana] 
laborables = [Lunes,Martes,Miercoles,Jueves,Viernes] 
pertenece :: (Eq a) => [a] -> a -> Bool 
pertenece [] _ = False 
pertenece (x:xs) e = x==e || pertenece xs e 
esLaborable :: DiaSemana -> Bool 
esLaborable dia = dia `elem` laborables 
--otra versión 
esLaborable2 dia = pertenece laborables dia 
