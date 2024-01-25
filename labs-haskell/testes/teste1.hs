-- 1

{-

Escreva uma definição da função maxpos :: [Int] -> Int que calcula o máximo dos inteiros positivos numa lista; se a lista não tiver números positivos o resultado deve ser 0.
Exemplos:

maxpos [1,2,3,4,5]      == 5
maxpos [-1,-2,-3,4,-5]  == 4
maxpos [2]              == 2
maxpos []               == 0

-}

maxpos :: [Int] -> Int
maxpos [] = 0
maxpos (h:t) = 
    if h >= maxpos t then h
    else maxpos t



-- 2

{-

Escreva uma definição da função dups :: [a] -> [a] que duplica os valores em posições alternadas duma lista (isto é, duplica o primeiro elemento, não duplica o segundo, duplica o terceiro, e assim sucessivamente).
Exemplos:

dups "abcdef"    == "aabccdeef"
dups [0,1,2,3,4] == [0,0,1,2,2,3,4,4]
dups []          == []


-}

dupsAux :: [a] -> [a]
dupsAux [] = []
dupsAux (h:t) = h:dups t

dups :: [a] -> [a]
dups [] = []
dups (h:t) = h:h:dupsAux t


-- 3

{-

A linguagem dos Ps é um jogo de palavras em que duplicamos cada vogal (letras 'a', 'e', 'i', 'o', 'u') e colocamos um 'p' entre elas; todos os outros carateres e letras ficam inalterados. Assim, por exemplo, a frase "ola, mundo!" fica "opolapa, mupundopo!". Escreva uma definição da função transforma :: String -> String que transforma uma frase para a linguagem dos Ps. Para simplificar, assuma que a frase não contém letras maiúsculas nem acentuadas.
Exemplos:

transforma "ola, mundo!"       = "opolapa, mupundopo!"
transforma "4 gatos e 3 ratos" = "4 gapatopos epe 3 rapatopos"

-}

transforma :: String -> String
transforma [] = []
transforma (h:t) 
    | elem h vogals = h:'p':h:transforma t
    | otherwise = h:transforma t
    where vogals = ['a', 'e', 'i', 'o', 'u']


-----------------------

type Vector = [Int]
type Matriz = [[Int]]


-- 4

{-

Defina a função transposta :: Matriz -> Matriz, que calcula a matriz transposta.
Exemplo:

transposta [[1,2], [3,4]]     = [[1,3], [2,4]]
transposta [[1,2,3], [4,5,6]] = [[1,4], [2,5], [3,6]]

-}

transposta :: Matriz -> Matriz
transposta topg = 
    if null (head topg)
        then  []
    else 
        map head topg : transposta (map tail topg)


-- 5

{-

Defina a função prodInterno :: Vector -> Vector -> Int, que calcula o produto interno de dois vectores (a soma dos produtos de elementos com o mesmo índice). Assume-se que ambos os vetores têm o mesmo comprimento.
Exemplo:

prodInterno [1,2,3] [4,3,2] = 16

-}

prodInterno :: Vector -> Vector -> Int
prodInterno v1 v2 = sum [el1*el2 | (el1,el2) <- zip v1 v2]


-- 6

{-

Defina a função prodMat :: Matriz -> Matriz -> Matriz, que calcula o produto de duas matrizes.
Exemplo:

prodMat [[1,2], [3,4]] [[2,3], [2,4]] = [[6,11], [14,25]]
(note que 6 = 1*2 + 2*2 e 11 = 1*3 + 2*4

-}





-----------------------------

data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)


-- 7

{-

Escreva uma definição da função alturas :: Arv a -> Arv Int que transforma uma árvore binária noutra com a mesma estrutura mas em que o valor de cada nó é dado pela sua altura (isto é, o maior comprimento dum caminho desse nó até uma folha).
Exemplo (com uma árvore de strings):

alturas (N "joão" (N "abel" F F) (N "pedro" F F)) =  N 2 (N 1 F F) (N 1 F F)

-}

altura :: Arv a -> Int
altura F = 0
altura (N _ F F) = 1
altura (N _ F dir) = 1 + altura dir
altura (N _ esq F) = 1 + altura esq
altura (N  _ esq dir) = 1 + max (altura esq) (altura dir)


alturas :: Arv a -> Arv Int
alturas F = F
alturas (N a esq dir) = N (currAltura) (alturas esq) (alturas dir)
    where currAltura = altura (N a esq dir)


-- 8

{-

Defina uma outra função equilibrada :: Arv a -> Bool que exprima a condição duma árvore ser equilibrada, isto é, as alturas das sub-árvores de cada nó diferem no máximo de 1 unidade.
Exemplo (com a mesma árvore):

equilibrada (N "joão" (N "abel" F F) (N "pedro" F F)) =  True
Texto da resposta Pergunta 8

-}

equilibrada :: Arv a -> Bool 
equilibrada F = True
equilibrada (N  a esq dir) = abs (altura esq - altura dir) <= 1 && equilibrada esq && equilibrada dir


-- 9

{-

Escreva a definição de uma função f, em Haskell, que tenha o tipo:

f :: (a -> b -> c) -> b -> a -> c

(nota: não complique - a função pretendida não é complicada!)

-}

f :: (a -> b -> c) -> b -> a -> c
f g x y = g y x

