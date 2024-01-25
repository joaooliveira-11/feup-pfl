import Data.Char (chr, ord)
import Data.Char (isUpper, isLower, isDigit)
import Data.List
import Distribution.SPDX (LicenseId(W3C_19980720))

-- 2.1)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs 

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs 

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 el = []
myReplicate count el = el : myReplicate (count -1) el

myIdx :: [a] -> Int -> a
myIdx [] _ = error "invalid index"
myIdx (h:t) 0 = h
myIdx (h:t) idx = myIdx t (idx-1)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (h:t) = 
    if el == h then True
    else myElem el t


-- 2.2)

myInterspace :: a -> [a] -> [a]
myInterspace _ [] = []
myInterspace _ [el] = [el]
myInterspace sep (h:t) = h:sep:myInterspace sep t

-- 2.3)

mdc :: Integer -> Integer -> Integer
mdc a b 
    | b == 0 = a
    | otherwise = mdc b (a `mod` b)

mdc1 :: Integer -> Integer -> Integer
mdc1 a 0 = a
mdc1 a b = mdc1 b (a `mod` b)

-- 2.4)

myInsert :: Ord a => a -> [a] -> [a]
myInsert el [] = [el]
myInsert el (h:t) =
    if el <= h then el:h:t
    else h:myInsert el t

myIsort :: Ord a => [a] -> [a]
myIsort [] = []
myIsort (h:t) = myInsert h (myIsort t)


-- 2.5)

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "invalid input"
myMinimum [el] = el
myMinimum (h:t) = 
    if h <= myMinimum t then h
    else myMinimum t


myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete el (h:t) =
    if el == h then t
    else h : myDelete el t

mysSort :: Ord a => [a] -> [a]
mysSort [] = []
mysSort list = minimum : mysSort (myDelete minimum list)
    where minimum = myMinimum list

-- 2.6)
sumSquare :: Integer -> Integer
sumSquare n = sum [el^2| el<-[1..n]]

-- 2.8)
dotprod :: [Float] -> [Float] -> Float
dotprod l1 l2 = sum [el1 * el2 | (el1, el2) <- zip l1 l2]

-- 2.9)
divprop :: Integer -> [Integer]
divprop n = [div | div<-[1..(n-1)], n `mod` div == 0]

-- 2.10)
perfeitos :: Integer -> [Integer]
perfeitos n = [perfeito | perfeito <-[1..n], sum(divprop perfeito) == perfeito]

--2.11)
pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x ^2 + y^2 == z^2]

-- 2.12)
primo :: Integer -> Bool
primo n = divs == [1]
    where divs = divprop n

-- 2.13)
mersennes :: [Int]
mersennes = [n | n <- [1..30], primo (2^n -1)]

-- 2.16)
concatComp :: [[a]] -> [a]
concatComp list = [el | sublist<- list, el <- sublist]
 
replicateComp :: Integer -> a -> [a]
replicateComp counter el = [el | _ <- [1..counter]]

indexComp :: [a] -> Int -> a
indexComp list idx = head [el | (el, pos )<- zip list [0..length list - 1], idx == pos]

-- 2.17)
forte :: String -> Bool
forte word = 
    length word >= 8 &&
    or [isUpper el | el <- word] &&
    or [isLower el | el <- word] && 
    or [isDigit el | el <- word] 

-- 2.19)
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) = h : nub1 newlist
    where newlist = [el | el <- t, el /= h]

-- 2.20)
transpose1 :: [[a]] -> [[a]]
transpose1 lists = [[sublist !! idx | sublist <- lists] | idx <- [0..maxidx]]
    where maxidx = length (head lists) -1

transpose2 :: [[a]] -> [[a]]
transpose2 lists =
    if null (head lists) then []
    else map head lists : transpose2 (map tail lists)

transpose3 :: [[a]] -> [[a]]
transpose3 lists =
    if null (head lists) then []
    else map (\sublist -> head sublist) lists : transpose2 (map (\sublist -> tail sublist)lists)

-- 2.21)
algarismosAux :: Int -> [Int]
algarismosAux 0 = []
algarismosAux n = n `mod` 10 : algarismosAux (n `div` 10)

algarismos :: Int -> [Int]
algarismos n = reverse (algarismosAux n)

-- 2.22)
toBitsAux :: Int -> [Int]
toBitsAux 0 = []
toBitsAux n = n `mod` 2 : toBitsAux (n `div` 2)

toBits :: Int -> [Int]
toBits n = reverse (toBitsAux n)

-- 2.23)
fromBits :: [Int] -> Int
fromBits list = sum [2^pos * el | (el, pos) <- zip list [size, size-1..0]]
    where size = length list -1

fromBits1 :: [Int] -> Int
fromBits1 [] = 0
fromBits1 (h:t)= h *2^multiplier + fromBits t
    where multiplier = (length (h:t)) -1


-- 2.24)
merge :: Ord a => [a] -> [a] -> [a]
merge [] l2 = l2
merge l1 [] = l1
merge (h1:t1) (h2:t2) = 
    if h1 <= h2 then h1 : merge t1 (h2:t2)
    else h2 : merge (h1:t1) t2

metades :: [a] -> ([a], [a])
metades list = (take half list, drop half list)
    where half = (length list) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [el] = [el]
msort list = merge (msort l1) (msort l2)
    where (l1,l2) = metades list

