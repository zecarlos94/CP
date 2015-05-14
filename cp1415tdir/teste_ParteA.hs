import LTree
import Cp

-- LER 

-- Testei tudo e passam os testes unitários, no entanto a função balance tem uma sub funçao generateT que está feita em haskell apenas, o que impede fazer a funçao balance numa linha.




-- Module testes sem aplicar diretamente as librarias dadas

-- LTree type
-- data LTree a = Leaf a | Fork (LTree a, LTree a) deriving (Show, Eq)
--

t = Fork (Fork (Leaf 10,Fork (Leaf 2,Fork (Leaf 5,Leaf 3))),Leaf 23) 

max' (a,b) = if (a > b) then a else b

--depth :: LTree a -> Integer
--depth (Leaf a) = 0
--depth (Fork (a,b)) = max'(1 + depth(a) , 1 + depth(b) )

depth = cataLTree (either zero (max'.(succ><succ)))

balanced (Leaf _) = True
balanced (Fork(t,t')) = balanced t && balanced t' && abs(depth t - depth t') <= 1

-- transformar em ??                         anaLTree -> maybe
generateT :: [a] -> LTree a
generateT [a] = Leaf a
generateT (h:t) = (insert h (generateT t))
		where insert x (Leaf a) = Fork(Leaf x, Leaf a)
		      insert x (Fork(a,b))
			|(depth a) > (depth b) = insert x b
			|otherwise = insert x a


-- transformado em cataLTree
--decomposeT :: LTree a -> [a]
--decomposeT (Leaf a) = [a]
--decomposeT (Fork(a,b)) = decomposeT(a) ++ decomposeT(b)


joint (a,b) = a ++ b

decomposeT = cataLTree (either singl joint)

-- Usar o cata e ana para compor a funçao numa linha
-- Nota nao é um hyloLTree pois o cata e o ana estao trocados
balance :: LTree a -> LTree a
balance = generateT.decomposeT

-- balance = generrateT . cataLTree (either singl joint)

-- balance = generateT . cataLTree (either singl joint)

-- Testes unitários (Verdadeiro -> Passou o teste)

--teste a balanced (usando o cataLTree depth)
test01 = balanced t == False

--teste a balance
test02 = balanced (balance t) == True
