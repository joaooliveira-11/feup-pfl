-- 1.1)
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b > c) && (a + c > b) && (b + c > a) 

-- 1.2)
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = 
    let s = (a + b + c) / 2
    in sqrt(s * (s - a) * (s - b) * (s - c))

areaTrianguloX :: Float -> Float -> Float -> Float
areaTrianguloX a b c = sqrt(s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- 1.3)
metades :: [a] -> ([a], [a])
metades xs = (take half xs, drop half xs)
    where half = length xs `div` 2

metadesx :: [a] -> ([a], [a])
metadesx list = 
    let half = length list `div`2
    in (take half list , drop half list)
    
-- 1.4) head, tail, length, take, drop e reverse.

--a) last function

last2 :: [a] -> a
last2 list = head (reverse list)

last3 :: [a] -> a
last3 list = (reverse list) !! 0

last4 :: [a] -> a
last4 list = head (drop (length list - 1) list)


--b) init function

init2 :: [a] -> [a]
init2 list = take (length list - 1) list

init3 :: [a] -> [a]
init3 list = reverse (tail (reverse list) )

init4 :: [a] -> [a]
init4 list = reverse (drop 1 (reverse list))

-- 1.5)

-- a)


binom :: Integer -> Integer -> Integer
binom n k = product [1..9] `div`product [1..k] * product[1..(n-k)]


-- 1.6)

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (r1, r2)
    where  
        r1 = -b + sqrt(b^2 - 4 * a * c) / (2 * a)
        r2 = -b - sqrt(b^2 - 4 * a * c) / (2 * a)

raizes1 :: Float -> Float -> Float -> (Float, Float)
raizes1 a b c = 
    let 
        r1 = -b + sqrt(b^2 - 4 * a * c) / (2 * a)
        r2 = -b - sqrt(b^2 - 4 * a * c) / (2 * a)
    in (r1,r2)


-- 1.7)

-- a) [Char]
-- b) (Char, Char, Char)
-- c) [(Bool, Char)]
-- d) ([Bool], [Char])
-- e) [ [a] -> [a] ]
--f) id: a -> a   not: Bool -> Bool  combination invalid


-- 1.8)

-- a)
segundo :: [a] -> a
segundo xs = head (tail xs)

-- b)
trocar :: (a,b) -> (b,a)
trocar (x, y) = (y, x)


-- c) 
par :: a -> b -> (a,b)
par x y = (x, y)

-- d) 
dobro :: Num a => a -> a
dobro x = 2 * x
 
-- e) 
metade :: Fractional a => a -> a
metade x = x/2

-- f)
minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'

-- g)
intervalo :: Ord a => a -> a -> a -> Bool
intervalo x a b = x >=  a && x <= b

-- h) 
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- i)
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 1.9)

classificaConditional :: Int -> String
classificaConditional grade =
    if grade <= 9 then "reprovado"
    else if grade <= 12 then "suficiente"
    else if grade <= 15 then "bom"
    else if grade <= 18 then "muito bom"
    else if grade <= 20 then "muito bom com distinção"
    else "nota invalida"

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
    where imc = peso / (altura ^2)

classificaImcGuards :: Float -> Float -> String
classificaImcGuards peso altura
    | imc < 18.5 = "baixo peso"
    | imc < 25 = "peso normal"
    | imc < 30 = "excesso de peso"
    | otherwise = "obesidade"
    where imc = peso * (altura ^2)

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

safetailConditional :: [a] -> [a] 
safetailConditional list=
    if (length list) == 0 then []
    else tail list

safetailConditional1 :: [a] -> [a] 
safetailConditional1 list =
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

curtaLength :: [a] -> Bool
curtaLength list = (length list) <= 2

curtaPatterns :: [a] -> Bool
curtaPatterns [] = True
curtaPatterns [_] = True
curtaPatterns [_,_] = True
curtaPatterns _ = False

curtaGuards :: [a] -> Bool
curtaGuards list
    | null list = True
    | length list == 1  = True
    | length list == 2  = True
    | otherwise = False

-- 1.15)
mediana :: Ord a => a -> a -> a -> a
mediana x y z
    | (x <= y && y <= z) || (z <= y && y <= x) = y
    | (y <= x && x <= z) || (z <= x && x <= y) = x
    | otherwise = z

medianaSum :: (Ord a, Num a) => a -> a -> a -> a
medianaSum x y z = x + y + z - max x (max y z) - min x (min y z)

-- 1.16) moodlez
