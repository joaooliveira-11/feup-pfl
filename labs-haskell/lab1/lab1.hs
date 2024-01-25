import Language.Haskell.TH (safe)
-- 1.1)

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo l1 l2 l3 = (l1 < l2 + l3) && (l2 < l1 + l3) && (l3 < l1 + l2)

-- 1.2)

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo l1 l2 l3 = sqrt(s * (s -l1) * (s-l2) * (s-l3))
    where s = (l1+l2+l3)/2

areaTriangulo2 :: Float -> Float -> Float -> Float
areaTriangulo2 a b c = 
    let s = (a + b + c) / 2
    in sqrt(s * (s - a) * (s - b) * (s - c))
                        
-- 1.3)

metades :: [a] -> ([a], [a])
metades list = (take half list, drop half list)
    where half = (length list) `div` 2


-- 1.4)
lastReverse :: [a] -> a
lastReverse list = head (reverse list)

lastTake :: [a] -> a
lastTake list = head (drop ((length list) -1) list)

lastTake2 :: [a] -> a
lastTake2 list = head (take 1 (reverse list))


-- 1.5)

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..(n-k)])

-- 1.6)

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (r1,r2)
    where r1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
          r2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)


raizes1 :: Float -> Float -> Float -> (Float, Float)
raizes1 a b c = 
    let
        r1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
        r2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)
    in (r1,r2)


-- 1.7)

-- a) [Char]
-- b) (Char, Char, Char)
-- c) [(Bool, Char)]
-- d) ([Bool], [Char])
-- e) [ [a] -> [a] ]
--f) id: a -> a   not: Bool -> Bool  combination invalid


-- 1.8

-- a) segundo :: [a]- > a
-- b) trocar :: (a,b) -> (b,a)
-- c) par :: a -> b -> (a,b)
-- d) dobro :: Num a => a -> a
-- e) metade :: Fractional a => a -> a
-- f) minuscula :: Char -> Bool
-- g) intervalo :: Ord a => a -> a -> a-> Bool
-- h) palindrome :: Eq  a =>  [a] -> Bool
-- i) twice :: (a -> a) -> a -> a

-- 1.9)

classificaCond :: Int -> String
classificaCond grade = 
    if grade <= 9 then "reprovado"
    else if grade <= 12 then "suficiente"
    else if grade <= 15 then "bom"
    else if grade <= 18 then "muito bom"
    else if grade <= 20 then "muito bom com distinção"
    else "unknown"

classificaGuards :: Int -> String
classificaGuards grade  
    | grade <= 9 = "repovado"
    | grade <= 12 = "suficiente"
    | grade <= 15 = "bom"
    | grade <= 18 = "muito bom"
    | grade <= 20 = "muito bom com distinção"
    | otherwise = "nota invalida"


-- 1.10)

classificaImcConditional :: Float -> Float -> String
classificaImcConditional peso altura = 
    if imc < 18.5 then "baixo peso"
    else if imc < 25 then "peso normal"
    else if imc < 30 then "excesso de peso"
    else "obesidade"
    where imc = peso / (altura^2)

classificaImcGuards :: Float -> Float -> String
classificaImcGuards peso altura 
    | imc < 18.5 = "baixo peso"
    | imc < 25 = "peso normal"
    | imc < 30 = "excesso de peso"
    | otherwise = "obesidade"
    where imc = peso / (altura^2)

-- 1.11)

max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)

min3 :: Ord a => a -> a -> a -> a
min3 a b c = min a (min b c)

-- 1.12)
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- 1.13)
safetailCond :: [a] -> [a]
safetailCond list = 
    if null list then []
    else tail list

safetailGuards :: [a] -> [a]
safetailGuards list 
    | null list = []
    | otherwise = tail list

safetailPatterns :: [a] -> [a]
safetailPatterns [] = []
safetailPatterns list = tail list

-- 1.14)
curta :: [a] -> Bool
curta list = length list <= 2

curtaPatterns :: [a] -> Bool
curtaPatterns [] = True
curtaPatterns [_] = True
curtaPatterns [_,_] = True
curtaPatterns _ = False

curtaGuards :: [a] -> Bool
curtaGuards list
    | null list = True
    | length list  == 1 = True
    | length list  == 2 = False
    | otherwise = False

-- 1.15)
mediana :: Ord a => a -> a -> a -> a
mediana a b c = 
    if ((a >= b && a <= c) || (a >= c && a <= b))  then a
    else if ((b >= a && b <= c) || (b >= c && b <= a))  then b
    else c

medianaSum :: Num a => a -> a -> a -> a
medianaSum x y z = x + y + z - max x (max y z) - min x (min y z)



{-
-- 1.15)
mediana :: Ord a => a -> a -> a -> a
mediana x y z
    | (x <= y && y <= z) || (z <= y && y <= x) = y
    | (y <= x && x <= z) || (z <= x && x <= y) = x
    | otherwise = z

medianaSum :: (Ord a, Num a) => a -> a -> a -> a
medianaSum x y z = x + y + z - max x (max y z) - min x (min y z)

-- 1.16) moodlez

-}