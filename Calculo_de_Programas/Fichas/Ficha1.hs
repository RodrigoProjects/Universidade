-- FICHA 1

-- Ex-2

length1 :: [a] -> Integer
length1 [] = 0
length1 (h:t) = 1 + length1 t

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h] 

-- Ex-3

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing:t) = catMaybes' t
catMaybes' (Just a:t) = [a] ++ catMaybes' t

-- Ex-4

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- Ex-5

data LTree a = Leaf a | Fork (LTree a, LTree a)

flatten' :: LTree a -> [a]
flatten' (Leaf a) = [a]
flatten' (Fork (a, b)) = flatten' a ++ flatten' b

mirror' :: LTree a -> LTree a 
mirror' (Leaf a) = Leaf a
mirror' (Fork (a,b)) = Fork (mirror' a, mirror' b)

fmap' :: (b -> a) -> LTree b -> LTree a 
fmap' f (Leaf x) = (Leaf (f x))
fmap' f (Fork (a,b)) = Fork (fmap' f a, fmap' f b)

-- Ex-6

lengthR :: [a] -> Integer
lengthR = foldr (\ x y -> 1 + y) 0

-- Ex-7

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

-- Ex-8


-- Função que aumenta o Inteiro por um se esse inteiro for maior que 0.

f :: [Integer] -> [Integer]
f s = [a + 1 | a <- s, a > 0]

f' :: [Integer] -> [Integer]
f' =  foldr (\ x xs -> if x>0 then (x+1):xs else xs) []                      

-- Ex-9

-- a)
mapR :: (a -> b) -> [a] -> [b]
mapR f  = foldr (\ x y -> (f x): y) [] 

-- b) Ambiguos
-- c) m :: [a] -> [[a]]
-- d) Trivial


