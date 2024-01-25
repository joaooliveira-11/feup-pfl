import Data.List (foldl1)
import Data.List (foldr1)
import Data.List (insert)
import Data.Char (isSpace)
import Data.Char (isAlpha)

-- 3.1)

listMapFilter :: [a] -> (a -> a) -> (a -> Bool) -> [a]
listMapFilter list f fil = map f (filter fil list)

-- 3.2)

dec2int :: [Int] -> Int
dec2int list = foldl (\acc currDigit -> acc * 10 + currDigit) 0 list


-- 3.3)
zipWith1 :: (a -> b -> c) ->  [a] -> [b] -> [c]
zipWith1 f _ [] = []
zipWith1 f [] _ = []
zipWith1 f (h1:t1) (h2:t2) = f h1 h2 : zipWith1 f t1 t2

-- 3.4)
myIsort :: Ord a => [a] -> [a]
myIsort list = foldr (\x acc -> insert x acc) [] list 

-- 3.5)

myMaximum :: Ord a => [a] -> a
myMaximum list = foldl1 (\acc x -> max acc x) list

myMaximum2 :: Ord a => [a] -> a
myMaximum2 list = foldl1 (\x acc -> max acc x) list

-- 3.6)
mdc :: Integral a => a -> a -> a
mdc a b = fst( until (\(x,y) -> y == 0) (\(x,y) -> (y, x `mod` y)) (a,b) )

-- 3.7)
myConcatTwo :: [a] -> [a] -> [a]
myConcatTwo l1 l2 = foldr (\x acc -> x : acc) l2 l1
--                = foldr (:) l2 l1

myConcat :: [[a]] -> [a]
myConcat list = foldr (++) [] list

myReverse :: [a] -> [a]
myReverse l = foldr (\x acc -> acc ++ [x]) [] l

myReverse1 :: [a] -> [a]
myReverse1 l = foldl (\acc x -> x : acc) [] l

myElem :: Eq a => a -> [a] -> Bool
myElem el list = any (\x -> x == el) list

-- 3.8)
palavras :: String -> [String]
palavras "" = [] 
palavras str = takeWhile (\x -> not (isSpace x)) str : palavras (dropWhile isSpace (dropWhile (\x -> not (isSpace x)) str ))


despalavras :: [String] -> String
despalavras list = foldr (\x acc -> x ++ " " ++ acc) [] list
