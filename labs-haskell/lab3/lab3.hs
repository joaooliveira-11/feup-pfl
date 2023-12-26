import Data.List (foldl1)


-- 3.1)
-- [f x | x ← xs, p x] s
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p xs = map f (filter p xs)

-- 3.2)
dec2int :: [Int] -> Int
dec2int list = foldl (\acc x -> acc * 10 + x) 0 list

-- 3.3)
-- zipWith :: (a → b → c) → [a] → [b] → [c]
-- zipWith f xs ys = [f x y | (x, y) ← zip xs ys

zipWithRec :: (a -> b -> c) -> [a] -> [b] -> [c] 
zipWithRec _ [] _ = [] 
zipWithRec _ _ [] = [] 
zipWithRec f (h1:t1) (h2:t2) = f h1 h2 : zipWithRec f t1 t2

-- 3.4)

{- from lab 2

-- 2.4)

-- a)
insert_2 :: Ord a => a -> [a] -> [a]
insert_2 a [] = [a]
insert_2 a (h:t)
    | a <= h = a : h : t
    | otherwise = h : insert_2 a t

-- b)
isort_2 :: Ord a => [a] -> [a]
isort_2 [] = []
isort_2 (h:t) = insert_2 h (isort_2 t)

isort :: Ord a ⇒ [a] → [a]
-}

insert_2 :: Ord a => a -> [a] -> [a]
insert_2 a [] = [a]
insert_2 a (h:t)
    | a <= h = a : h : t
    | otherwise = h : insert_2 a t

isortFoldr :: Ord a => [a] -> [a]
isortFoldr list = foldr insert_2 [] list


-- 3.6)

untilMdc :: Integer -> Integer -> Integer
untilMdc a b = fst(until (\(_, b) -> b == 0) (\(a, b) -> (b, a `mod` b)) (a, b))

-- 3.7)

-- a)
