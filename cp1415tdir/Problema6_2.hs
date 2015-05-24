module Problema6_2 where

import List
import Probability



-- Exemplo fornecido no enunciado

padd (a, b) = D [(a + b, 0.9), (a − b, 0.1)]
pcataList :: (Either () (a, b) -> Dist b) -> [a] -> Dist b

d4 = pcataList [pzero, padd] [20, 10, 5] where pzero = return · zero

--pcataList (either (return.zero) padd) [20,10,5]


{-
pcataList :: (Either () (a, b) -> Dist b) -> [a] -> Dist b

pwordAux a x = D [((a==x && a/="Stop"), 0.05), (a=="Stop", 0.1) , (a/=x,0.85)]
			
pwords [a] x = (pwords (tail a) x).(pwordAux a x) 

gene= either pzero pwords
    where pzero = return · zero


transmitir = pcataList gene    

-- O resultado do test07 será D [(["a", "b"], 0.95), (["b"], 0.05)]
test07 = gene (i2 ("a", ["b"])) 

--Resultado a obter 
transmitir (words "Vamos atacar hoje")

-}

--pcataList (either (return.zero) pwords) (words "Vamos atacar hoje")

