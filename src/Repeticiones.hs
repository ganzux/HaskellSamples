module Repeticiones where

repeticionesAux :: [Int] -> [Int] -> [Int] -> ([Int],[Int])

repeticionesAux [] repes noRepes = (repes, noRepes)

repeticionesAux (x:xs) repes noRepes
     | not(existe repes x) && existe xs x	  = repeticionesAux xs (x:repes) noRepes
     | not(existe repes x) && not(existe xs x)= repeticionesAux xs repes (x:noRepes)
     | otherwise = repeticionesAux xs repes noRepes

existe:: [Int] -> Int -> Bool
existe [] _ = False
existe (x:xs) n = if n==x then True else existe xs n
