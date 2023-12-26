import Data.Char (chr, ord)
import Data.Char (isUpper, isLower, isDigit)

-- 2.2)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse a (x:xs) = x : a : intersperse a xs

-- 2.3)

mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

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



-- 2.5)

-- a)
minimum_2 :: Ord a => [a] -> a
minimum_2 [x] = x
minimum_2 (h:t)
    | h < minimum_2 t = h
    | otherwise = minimum_2 t

-- b)
delete_2 :: Eq a => a -> [a] -> [a]
delete_2 _ [] = []
delete_2 a (h:t)
    | a == h = t
    | otherwise = h : delete_2 a t

-- c)

ssort_2 :: (Ord a, Eq a) => [a] -> [a]
ssort_2 [] = []
ssort_2 xs = minlement : ssort_2 (delete_2 minlement xs)
    where minlement = minimum_2 xs

-- 2.6)
listComp :: Integer
listComp = sum [x ^2 | x <- [1..100]]

-- 2.8)
dotprod :: [Float] -> [Float] -> Float
dotprod l1 l2 = sum [x * y | (x,y) <- zip l1 l2]

-- 2.9)
divprop :: Integer -> [Integer]
divprop x = [y | y <- [1..x-1], x `mod` y == 0]

-- 2.10)
perfeitos :: Integer -> [Integer]
perfeitos x = [y | y <- [1..x], sum (divprop y) == y]

-- 2.11)
pitagoricos :: Integer -> [(Integer ,Integer ,Integer)] 
pitagoricos n = [(x,y,z) | x <- [1..n],  y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 2.12)
primo :: Integer -> Bool
primo x = length (divprop x) == 1

-- 2.13)
mersennes :: [Int]
mersennes = [x | x  <- [1..30], primo (2 ^x -1)]

-- 2.15)

-- Converte letras em inteiros 0..25 e vice-versa
letraInt :: Char -> Int
letraInt c = ord c - ord 'A'

intLetra :: Int -> Char
intLetra n = chr (n + ord 'A')

maiúscula :: Char -> Bool
maiúscula x = x >='A' && x <='Z'

-- Efectuar um deslocamento de k posições
desloca :: Int -> Char -> Char
desloca k x | maiúscula x = intLetra ((letraInt x + k)`mod`26)
            | otherwise   = x

-- Repetir o deslocamento para toda a cadeia de caracteres.
cifrar :: Int -> String -> String
cifrar k xs = [desloca k x | x<-xs]

-- 2.16)
concat_2 :: [[a]] -> [a]
concat_2 mainlist =  [el | sublist <- mainlist, el <- sublist]

replicate_2 :: Integer -> a -> [a]
replicate_2 n el = [el | _Counter <- [1..n]]

-- (!!)
listIndex :: [a] -> Int -> a
listIndex list idx = head [el | (el, idx2) <- zip list [0..(length list) - 1], idx2 == idx]

-- 2.17)
forte :: String -> Bool
forte senha =
  length senha >= 8 &&
  or [isUpper c | c <- senha] &&
  or [isLower c | c <- senha] &&
  or [isDigit c | c <- senha]


-- 2.19)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (h:t) = h : nub [y | y <- t, y /= h]

-- 2.20)
