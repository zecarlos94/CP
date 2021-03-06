module TLTree where

import Cp
import Data.Monoid

-- (1) Datatype definition -----------------------------------------------------

type Tri = (Point, Side)
type Side = Int
type Point = (Int, Int)

--type Tri = ((Int, Int), Int)
--data TLTree = ((Int, Int), Int) ((Int, Int), Int) | Nodo TLTree TLTree TLTree deriving (Show, Eq)

        --construtor elementar
data TLTree a = Tri Tri | Nodo (TLTree a, TLTree a, TLTree a) deriving (Show, Eq)

--Each Leaf contains the bottom left vertex coordinates and the side of the peccaries in the same triangle.

inTLTree :: Either (Tri Tri) (TLTree a, TLTree a, TLTree a) ->  TLTree a
inTLTree = either (const Tri Tri) Nodo

outTLTree :: TLTree a -> Either (Tri Tri) (TLTree a, TLTree a, TLTree a)
outTLTree (Tri Tri) = i1 ()
outTLTree (Nodo (a,b,c)) = i2 (a,b,c)

-- (2) Ana + cata + hylo -------------------------------------------------------

recTLTree f = id -|- (f >< f >< f)

cataTLTree a = a . (recTLTree (cataTLTree a)) . outTLTree

anaTLTree f = inTLTree . (recTLTree (anaTLTree f) ) . f

hyloTLTree a c = cataTLTree a . anaTLTree c

baseTLTree g f =  g -|- (f >< f >< f)

-- recTLTree f = baseTLTree id f

-- (3) Map ---------------------------------------------------------------------

instance Functor TLTree
         where fmap f = cataTLTree ( inTLTree . baseTLTree f id )

-- (4) Examples ----------------------------------------------------------------

--depth :: TLTree a -> Int
--depth (Tri Tri) = 0
--depth (Nodo (a,b,c)) = max3(1 + depth(a) , 1 + depth(b) , 1 + depth(c))

depthTLTree :: TLTree b -> Int
depthTLTree = cataTLTree (either zero (max3.(succ><succ><succ)))

-- (4.0) Inversion (mirror) ----------------------------------------------------

invTLTree :: TLTree b -> TLTree b
invTLTree = cataTLTree (inTLTree . (id -|- swap3))

{-- Recall the pointwise version:
invTLTree (Tri Tri) = Tri Tri
invTLTree (Nodo (a,b,c)) = Nodo ((invTLTree c),(invTLTree b),(invTLTree a))
--}

-- (4.1) Counting --------------------------------------------------------------

countTLTree :: TLTree b -> Int
countTLTree = cataTLTree (either one succ.add3)

-- (4.2) Serialization ---------------------------------------------------------

tipsTLTree :: TLTree b -> [b]
tipsTLTree = cataTLTree (either Tri concat3)
   where concat3 (a,b,c) = a ++ b ++ c

-- (4.3) sierpinski triangles--------------------------------------------------------------

sierpinski :: Tri -> Int -> [Tri]
sierpinski t = apresentaSierp . (geraSierp t)

-- (4.3.1) geraSierp (Auxiliary) --------------------------------------------------------------

{-
geraSierp :: Tri -> Int -> TLTree
geraSierp t 0 = Tri t
geraSierp ((x , y), s) n = let s0 = s ÷ 2
                           in Nodo (geraSierp ((x , y), s0) (n − 1)) (geraSierp ((x + s0, y), s0) (n − 1)) (geraSierp ((x , y + s0), s0) (n − 1))
-}

-- (4.3.1) Redefinition of geraSierp (Auxiliary) as ana--------------------------------------------------------------

geraSierp g = anaTLTree ((g -|- tripleDec) . outTLTree)
   where tripleDec a = (a - 1, a - 1, a - 1)

-- (4.4) Showing TLTree as a list (apresentaSierp) ----------------------------------------------------
{-
apresentaSierp :: TLTree -> [Tri]
apresentaSierp (Tri t) = [t]
apresentaSierp (Nodo (a,b,c)) = (apresentaSierp a) ++ (apresentaSierp b) ++ (apresentaSierp c)
-}

-- (4.4) Redefinition of apresentaSierp as cata--------------------------------------------------------------

apresentaSierp g = cataTLTree (either singl concat3)
   where concat3 (a,b,c) = a ++ b ++ c

--ts = geraSierp tri 5 where tri = ((0, 0), 256) for testings

-- (4.5) Library X3d, using X3DOM. x3dom file in two steps: ----------------------------------------------------

--Drawing triangles, using:

drawTriangle :: ((Int, Int), Int) -> String
drawTriangle = undefined

--Ending the file with the tags of beggining and ending:

finalize :: String -> String
finalize = undefined

--allows you to see the x3dom file in a browser, but you might need some modifications in order to make it functional in your current browser.

render html = do {writeFile "_.html" html; system "open _.html"}


---------------------------- Auxiliary Functions -----------------------------

--Calculates maximum number among those three arguments

max3 :: (Ord a) => Int -> Int -> Int -> Int
max3 x y z | (x>=y && x>=z) = x
           | (y>=x && y>=z) = y
           | (z>=x && z>=y) = z

--Sum of those three arguments

add3 :: (Ord a) => Int -> Int -> Int -> Int
add3 x y z = x + y + z

--Swap of those three arguments

swap3 :: (Ord a) => TLTree a -> TLTree a
swap3 (Tri a) = (Tri a)
swap3 (Nodo (x,y,z)) = (Nodo (z,y,x))

---------------------------- end of library ----------------------------------
