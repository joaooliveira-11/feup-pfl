import Data.List (foldl1)
import Data.List (foldr1)
import Data.List (insert)
import Data.Char (isSpace)

-- 3.1)

-- [f x | x ← xs, p x] 

myFilter :: (a -> a) -> (a -> Bool) -> [a] -> [a]
myFilter func filt list = map func (filter filt list)

-- 3.2)

myDec2int :: [Int] -> Int
myDec2int list = foldl (\acc x -> acc * 10 + x) 0 list


-- 3.3)
zipWithRec :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRec _ [] _ = []
zipWithRec _ _ [] = []
zipWithRec f (h1:t1) (h2:t2) = f h1 h2 : zipWithRec f t1 t2

-- 3.4)
myInsert :: Ord a => a -> [a] -> [a]
myInsert el [] = [el]
myInsert el (h:t) = 
    if el <= h then el : h : t
    else h: myInsert el t 

myIsort :: Ord a => [a] -> [a]
myIsort [] = []
myIsort (h:t) = myInsert h (myIsort t)

foldIsort :: Ord a =>  [a] -> [a]
foldIsort list = foldr myInsert [] list


foldIsort1 :: Ord a =>  [a] -> [a]
foldIsort1 list = foldr (\x acc -> myInsert x acc) [] list


-- 3.5)

maximumLeft :: Ord a => [a] -> a
maximumLeft list = foldl1 (\acc x -> max acc x) list

maximumRight :: Ord a => [a] -> a
maximumRight list = foldr1 (\x acc -> max acc x) list

minimumLeft :: Ord a => [a] -> a
minimumLeft list = foldl1 (\acc x -> min acc x) list

minimumRight :: Ord a => [a] -> a
minimumRight list = foldr1 (\x acc -> min acc x) list

-- 3.6)


-- 3.7)

-- a)
concat2Foldr :: [a] -> [a] -> [a]
concat2Foldr list1 list2 = foldr (\x acc -> x : acc) list2 list1

-- b)
concatFoldr :: [[a]] -> [a]
concatFoldr lists = foldr (\x acc -> concat2Foldr x acc) [] lists

-- c)
reverseFoldr :: [a] -> [a]
reverseFoldr list = foldr (\x acc -> acc ++ [x]) [] list

-- d)
reverseFoldl :: [a] -> [a]
reverseFoldl list = foldl (\acc x -> [x] ++ acc) [] list

-- e)
elemAny :: Eq a => a -> [a] -> Bool
elemAny el list = any (\x -> x == el) list


-- 3.8)
