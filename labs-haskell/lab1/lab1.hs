-- 1.1)
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b > c) && (a + c > b) && (b + c > a) 

-- 1.2)
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = 
    let s = (a + b + c) / 2
    in sqrt(s * (s - a) * (s - b) * (s - c))

-- 1.3)
metades :: [a] -> ([a], [a])
metades xs = (take half xs, drop half xs)
    where half = length xs `div` 2

-- 1.4) head, tail, length, take, drop e reverse.

--a) last function

last_reverse :: [a] -> a
last_reverse xs = head (reverse xs)

last_drop :: [a] -> a
last_drop xs = head(drop (length xs - 1) xs)

--b) init function

init_take :: [a] -> [a]
init_take xs = take (length xs - 1) xs

init_reverse :: [a] -> [a] 
init_reverse xs = reverse(drop 1 (reverse xs) )

init_tail :: [a] -> [a]
init_tail xs = reverse(tail (reverse xs))


-- 1.5)

-- a)

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..(n-k)])


-- 1.6)

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = 
    ((-b + sqrt(b^2 - 4*a*c)) / (2*a)
    ,
    (-b - sqrt(b^2 - 4*a*c)) / (2*a))


-- 1.7)

-- a) [Char]
-- b) (Char, Char, Char)
-- c) [(Bool, Char)]
-- d) ([Bool], [Char])
-- e) [ [a] -> [a] ]
--f) id: a -> a   not: Bool -> Bool  combination invalid


-- 1.8)

-- a)  segundo xs = head (tail xs)
--     segundo :: [a] -> a

-- b) trocar (x, y) = (y, x)
--    trocar :: (a,b) -> (b,a)

-- c) par x y = (x, y)
--    par :: a -> b -> (a,b)

-- d) dobro x = 2 ∗ x
--    dobro :: Num a => a -> a

-- e)  metade x = x/2
--     metade :: Fractional a => a -> a

-- f) minuscula x = x ≥ 'a' && x ≤ 'z'
--    minuscula :: Char -> Bool

-- g) intervalo x a b = x ≥ a && x ≤ b
--    intervalo :: Ord a => a -> a -> a -> Bool

-- h) palindromo xs = reverse xs == xs
--    palindromo :: Eq a => [a] -> Bool


-- 1.9)

classificaCond :: Int -> String
classificaCond x = 
    if x <= 9 then "reprovado"
    else if x <= 12 then "suficiente"
    else if x <= 15 then "bom"
    else if x <= 18 then "muito bom"
    else "muito bom com distincao"

classificaGuard :: Int -> String
classificaGuard x
    | x <= 9 = "reprovado"
    | x <= 12 = "suficiente"
    | x <= 15 = "bom"
    | x <= 18 = "muito bom"
    | otherwise = "muito bom com distincao"

-- 1.10)

classificaIMC :: Float -> Float -> String
classificaIMC peso altura
    | imc < 18.5 = "baixo peso"
    | imc < 25 = "peso normal"
    | imc < 30 = "excesso de peso"
    | otherwise = "obesidade"
    where imc = peso / altura^2

-- 1.11)

max3 :: Ord a => a -> a -> a -> a
max3 x y z = max x (max y z)

min3 :: Ord a => a -> a -> a -> a
min3 x y z = min x (min y z)

-- 1.12)

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

-- 1.13)

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail_guard :: [a] -> [a]
safetail_guard xs
    | null xs = []
    | otherwise = tail xs

safetailPattern :: [a] -> [a]
safetailPattern [] = []
safetailPattern (_:xs) = xs

-- 1.14)

curtaLength :: [a] -> Bool
curtaLength xs = length xs <= 2

curtaPattern :: [a] -> Bool
curtaPattern [] = True
curtaPattern [_] = True
curtaPattern [_,_] = True
curtaPattern _ = False

-- 1.15)

mediana :: Ord a => a -> a -> a -> a
mediana x y z
    | (x <= y && y <= z) || (z <= y && y <= x) = y
    | (y <= x && x <= z) || (z <= x && x <= y) = x
    | otherwise = z

medianaSum :: (Ord a, Num a) => a -> a -> a -> a
medianaSum x y z = x + y + z - max x (max y z) - min x (min y z)

-- 1.16) moodle