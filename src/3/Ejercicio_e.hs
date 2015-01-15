module Ejercicio_e where
{--
Dada el siguiente tipo de datos recursivo que representa expresiones aritméticas: 

data Expr = Valor Integer 
    |Expr :+: Expr 
    |Expr :-: Expr 
    |Expr :*: Expr deriving Show

 e.1) Se pide una función para calcular el valor deuna expresión. 
 e.2) Se pide una función para calcular el número de constantes de una expresión 
 -}

data Expr = Valor Integer 
    |Expr :+: Expr 
    |Expr :-: Expr 
    |Expr :*: Expr deriving Show

valorDe :: Expr ->Integer 
valorDe (Valor n) = n 
valorDe (e1 :+: e2) = valorDe e1 + valorDe e2 
valorDe (e1 :-: e2) = valorDe e1 - valorDe e2 
valorDe (e1 :*: e2) = valorDe e1 * valorDe e2

numConsts :: Expr ->Integer 
numConsts (Valor n) = 1 
numConsts (e1 :+: e2) = numConsts e1 + numConsts e2
numConsts (e1 :-: e2) = numConsts e1 + numConsts e2
numConsts (e1 :*: e2) = numConsts e1 + numConsts e2
