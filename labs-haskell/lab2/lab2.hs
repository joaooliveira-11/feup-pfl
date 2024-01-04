import Data.Char (chr, ord)
import Data.Char (isUpper, isLower, isDigit)
import Data.List
import Distribution.SPDX (LicenseId(W3C_19980720))

-- 2.1)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (h:t) = h && myAnd t

myOr :: [Bool] -> Bool
myOr [] = False
myOr (h:t) = h || myOr t

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (h:t) = h ++ myConcat t

myConcat1 :: [[a]] -> [a]
myConcat1 lists = [el | list <- lists, el <- list]

myReplicate :: Int -> a -> [a]
myReplicate 0 n = []
myReplicate counter n = [n] ++ myReplicate (counter - 1) n 

myReplicate1 :: Int -> a -> [a]
myReplicate1 counter n = [n | _ <- [1..counter]]

myIndex :: [a] -> Int -> a
myIndex (x:_) 0  = x 
myIndex (_:xs) n = myIndex xs (n - 1)
myIndex _ _      = error "error"

-- 2.2)
intersperse1 :: a -> [a] -> [a]
intersperse1 sep [] = []
intersperse1 _ [x] = [x]
intersperse1 sep (h:t) = h : sep : intersperse1 sep t  

-- 2.3)
mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

-- 2.4)

-- a)
myInsert :: Ord a => a -> [a] -> [a]
myInsert el [] = [el]
myInsert el (h:t) = 
    if el <= h then el : h : t
    else h: myInsert el t 

-- b)
myIsort :: Ord a => [a] -> [a]
myIsort [] = []
myIsort (h:t) = myInsert h (myIsort t)


-- 2.5)

-- a)
myMinimum :: Ord a => [a] -> a
myMinimum [el] = el
myMinimum (h:t) =
    if h < myMinimum t then h
    else myMinimum t

-- b)
myDelete :: Eq  a=>  a -> [a] -> [a]
myDelete el [] = []
myDelete el (h:t) = 
    if el == h then t
    else h : myDelete el t  

-- c)
mySsort :: Ord a => [a] -> [a]
mySsort [] = []
mySsort list = currentMin : mySsort (myDelete currentMin list)
    where currentMin = myMinimum list 

-- 2.6)
mySquareSum :: Integer
mySquareSum = sum [x ^ 2 | x <- [1..100]]

-- 2.8) 
dotprod :: [Float] -> [Float] -> Float
dotprod l1 l2 = sum [x1 * x2 | (x1,x2) <- zip l1 l2]

-- 2.9)
divprop :: Integer -> [Integer]
divprop n = [div | div <- [1..n-1], n `mod` div == 0]

-- 2.10)
perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1..n], x == sum (divprop x)]

-- 2.11)
pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2 == z^2]

-- 2.12)
primo :: Integer -> Bool
primo n = (divprop n) == [1]

-- 2.13)
mersennes :: [Int]
mersennes = [n | n <- [1..30], primo (2 ^ n - 1)]

-- 2.16)

concatComp :: [[a]] -> [a]
concatComp lists = [el | list <- lists, el <- list]

replicateComp :: Integer -> a -> [a]
replicateComp counter el = [el | _ <- [1..counter]]

indexComp :: [a] -> Int -> a
indexComp list idx = head [el | (el, pos) <- zip list [0..length list] , pos == idx]


-- 2.17)
forte :: String -> Bool
forte string = 
    length string >= 8 &&
    or [isUpper c| c <- string] &&
    or [isLower c | c <- string] && 
    or [isDigit c | c <- string]


-- 2.19
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) = h : nub1 [e | e <- t, not (e == h)]  -- e /= b

-- 2.20)



-- 2.21)

algarismosAux :: Int -> [Int]
algarismosAux 0 = []
algarismosAux n = alg : algarismosAux rest
    where alg = n `mod` 10
          rest = n `div` 10


algarismos :: Int -> [Int]
algarismos n = reverse (algarismosAux n)

-- 2.22)

toBitsAux :: Int -> [Int]
toBitsAux 0 = []
toBitsAux n = bit : toBitsAux rest
    where
        bit = n `mod` 2
        rest = n `div` 2

toBits :: Int -> [Int]
toBits n = reverse (toBitsAux n)

-- 2.23) 

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (h:t)= h *2^multiplier + fromBits t
    where multiplier = (length (h:t)) -1

-- 2.24)
merge :: Ord a => [a] -> [a] -> [a]
merge [] l2 = l2
merge l1 [] = l1
merge (h:t) (h1:t1) =
    if h <= h1 then h : merge t (h1:t1)
    else h1 : merge (h:t) t1

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

