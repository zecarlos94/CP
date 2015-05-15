module TLTree where

import Cp
import Data.Monoid

-- (1) Datatype definition -----------------------------------------------------

type Tri = (Point, Side)
type Side = Int
type Point = (Int, Int)

--type Tri = ((Int, Int), Int)
--data TLTree = ((Int, Int), Int) ((Int, Int), Int) | Nodo TLTree TLTree TLTree deriving (Show, Eq)

            --Leaf
data TLTree = Tri Tri | Nodo TLTree TLTree TLTree deriving (Show, Eq)

--Each Leaf contains the bottom left vertex coordinates and the side of the peccaries in the same triangle.

type (Tri Tri) = t
type (TLTree TLTree TLTree) = ttt

inTLTree = either t Nodo

outTLTree :: TLTree a -> Either t ttt
outTLTree (Tri Tri)      = i1(t) 
outTLTree (Nodo TLTree TLTree TLTree) = i2(ttt)

-- (2) Ana + cata + hylo -------------------------------------------------------

recTLTree f = 

cataTLTree a = a . (recTLTree (cataTLTree a)) . outTLTree

anaTLTree f = inTLTree . (recTLTree (anaTLTree f) ) . f

hyloTLTree a c = cataTLTree a . anaTLTree c

baseTLTree g f = 

-- recTLTree f = baseTLTree id f

-- (3) Map ---------------------------------------------------------------------

instance Functor TLTree
         where fmap f = cataTLTree ( inTLTree . baseTLTree f id )

-- (4) Examples ----------------------------------------------------------------

-- (4.0) Inversion (mirror) ----------------------------------------------------

invTLTree = 


-- (4.1) Counting --------------------------------------------------------------

countTTLTree =

-- (4.2) Serialization ---------------------------------------------------------

tips = 


-- (4.3) sierpinski triangles--------------------------------------------------------------

sierpinski :: Tri -> Int -> [Tri]
sierpinski t = apresentaSierp · (geraSierp t)

-- (4.3.1) geraSierp (Auxiliary) --------------------------------------------------------------
{-
geraSierp :: Tri -> Int -> TLTree
geraSierp t 0 = Tri t
geraSierp ((x , y), s) n = let s0 = s ÷ 2
                           in Nodo (geraSierp ((x , y), s0) (n − 1)) (geraSierp ((x + s0, y), s0) (n − 1)) (geraSierp ((x , y + s0), s0) (n − 1))
-}

-- (4.3.1) Redefinition of geraSierp (Auxiliary) as ana--------------------------------------------------------------

geraSierp g =

-- (4.4) Showing TLTree as a list (apresentaSierp) ----------------------------------------------------
{-
apresentaSierp :: TLTree -> [Tri]
apresentaSierp (Tri t) = [t ]
apresentaSierp (Nodo a b c) = (apresentaSierp a) ++ (apresentaSierp b) ++ (apresentaSierp c)
-}

-- (4.4) Redefinition of apresentaSierp as cata--------------------------------------------------------------

apresentaSierp g =


-- (4.3) Redefinition of sierpinski as hylo--------------------------------------------------------------  

sierpinski g =

--ts = geraSierp tri 5 where tri = ((0, 0), 256) for testings

-- (4.5) Library X3d, using X3DOM. x3dom file in two steps: ----------------------------------------------------

--Drawing triangles, using:
drawTriangle :: ((Int, Int), Int) -> String

--Ending the file with the tags of beggining and ending:
finalize :: String -> String

--allows you to see the x3dom file in a browser, but you might need some modifications in order to make it functional in your current browser.
render html = do {writeFile "_.html" html; system "open _.html"}


---------------------------- end of library ----------------------------------