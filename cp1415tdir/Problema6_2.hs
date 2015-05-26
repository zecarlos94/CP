module Problema6_2 where

import Data.List
import System.Process
import Cp
import List 
import Nat  
import Exp
import BTree
import LTree
--import TLTree
import X3d
import Control.Parallel.Strategies
import Probability hiding (cond)
import System.Environment( getArgs )


{- Exemplo fornecido no enunciado

padd (a, b) = D [(a + b, 0.9), (a - b, 0.1)]

pcataList::Either () (a, b) -> Dist b -> [a] -> Dist b

d4 = pcataList [pzero, padd] [20, 10, 5] where pzero=return.zero




pwordAux::a->a->Dist a
pwordAux a x = D [((a==x && a/="Stop"), 0.05), (a=="Stop", 0.1) , (a/=x,0.85)]
            
pwords::[a]->a->Dist a
pwords [] x = D []
pwords (a:t) x = (pwords t x).(pwordAux a x)

gene= either pzero pwords
    where pzero = return.zero

pcataList :: (Either () (a, b) -> Dist b) -> [a] -> Dist b

transmitir = pcataList gene    

-- O resultado do test07 será D [(["a", "b"], 0.95), (["b"], 0.05)]
--test07 = gene (i2 ("a", ["b"])) 

--Resultado a obter 
--transmitir (words "Vamos atacar hoje")

d1 :: Dist Char
d1 = D [('A',0.02),('B',0.12),('C',0.29),('D',0.35),('E',0.22)]

d2 = uniform (words "Uma frase de cinco palavras")
-}
--pcataList :: (Either () (a, b) -> Dist b) -> [a] -> Dist b

--transmitir = D [("Vamos",0.02),("atacar",0.12),("hoje",0.29),("stop",0.29)]


padd(a,b) = D [(a+b,0.9),(a-b,0.1)]

d4 = pcataList (either pzero padd) [20,10,5] where pzero = return . zero

\begin{code}
pwordAux::a->a->Dist a
pwordAux a x = D [((a==x && a/="Stop"), 0.05), (a/=x,0.85)]
            
pwords::[a]->a->Dist a
pwords (a:t) x = (pwords t x).(pwordAux a x)
\end{code}

\begin{code}
transmitir = pcataList (either stop pwords) (words "Vamos atacar hoje") 
\end{code}


