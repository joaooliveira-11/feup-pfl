data Arv a = Vazia | No a (Arv a) (Arv a)
    deriving Show

-- 4.1)

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No n esq dir) = sumArv esq + n + sumArv dir


-- 4.2)

listar :: Arv a -> [a]
listar Vazia = []
listar (No n esq dir) = listar dir ++ [n] ++ listar esq

-- 4.3)

nivel :: Int -> Arv a -> [a] 
nivel _ Vazia = []
nivel 0 (No a esq dir) = [a]
nivel n (No a esq dir) = nivel nextLvl esq ++ nivel nextLvl dir
    where nextLvl = n -1

-- 4.5)

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No a esq dir) = No (f a) (mapArv f esq) (mapArv f dir)

-- 4.6)

mais_dir :: Arv a -> a
mais_dir Vazia = error "empty tree"
mais_dir (No n _ Vazia) = n
mais_dir (No a esq dir) = mais_dir dir

remover :: Ord a => a -> Arv a -> Arv a
remover _ Vazia = Vazia
remover rem (No a esq Vazia) 
    | a == rem = esq
remover rem (No a Vazia dir) 
    | a == rem = dir
remover rem (No a esq dir) = 
    if a == rem then (No  replace (remover replace esq) dir)
    else (No a (remover rem esq) (remover rem dir))
    where replace = (mais_dir esq)
