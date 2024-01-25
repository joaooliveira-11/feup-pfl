type Species = (String, Int)
type Zoo = [Species]

-- 1

{-

Implement isEndangered :: Species -> Bool, which receives a species and determines if it is endangered. A species is considered endangered
if there are 100 or less individuals in the zoo.

-}

isEndangered :: Species -> Bool
isEndangered (_name, nr)
    | nr <= 100 = True
    | otherwise = False


-- 2

{-

Implement updateSpecies :: Species -> Int -> Species, which, given a Species and an amount of newborn babies, returns a new instance of
Species with the updated population.

-}

updateSpecies :: Species -> Int -> Species
updateSpecies (name, nr) newBorns = (name, newNr)
    where newNr = nr + newBorns


-- 3

{-

Implement filterSpecies :: Zoo -> (Species -> Bool) -> Zoo, which, given the list species of a zoo and a predicate (i.e. a function that performs
a test on each species), returns the sublist of species that satisfy the predicate. The order of the species in the result must be the same as in
the input.
Constraint: You must solve this exercise using recursion. List comprehensions and higher-order functions (such as map and filter) are
prohibited.

-}

filterSpecies :: Zoo -> (Species -> Bool) -> Zoo
filterSpecies [] _ = []
filterSpecies (h:t) cond = 
    if cond h then h:filterSpecies t cond
    else filterSpecies t cond


-- 4

{-

Implement countAnimals :: Zoo -> Int, which, given the list of species of a zoo, counts the total population of the zoo.
Constraint: You must solve this exercise using higher-order functions. Recursion and list comprehensions are prohibited.

-}

countAnimals :: Zoo -> Int
countAnimals zoo = foldl (\acc (_name, count) -> acc + count) 0 zoo

countAnimals' :: Zoo -> Int
countAnimals' zoo = sum( map (\(_name, count) -> count) zoo )


-- 5

{-

Implement substring :: (Integral a) => String -> a -> a -> String, which returns the substring of a given string between an initial and final
index (the character on the final index should also be included in the result; both indices are within bounds). Consider that the indices start
at 0.
Constraint: You must solve this exercise using a list comprehension. Recursion and higher-order functions are prohibited.

-}

substring :: (Integral a) => String -> a -> a -> String
substring str lowb uppb = [(!!) str (fromIntegral idx) | idx <- [lowb..uppb]]

-- 6

{-

Implement hasSubstr :: String -> String -> Bool, which determines if the first string argument contains the second string argument (i.e. the
second argument is a substring of the first one).

-}


hasSubstr :: String -> String -> Bool
hasSubstr str1 str2 = or [str2 == substring str1 lowb uppb | lowb <- [0..size] , uppb <- [0..size]]
    where size = length str1 -1


-- 7

{-

Implement sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo), which divides the species of the Zoo into a pair of sublists. The first sublist
stores all the species whose name contains the string argument, while the second list has the remaining species. The order of the species in
each list of the resulting pair must match the input list.

-}

sortSpeciesWithSubstr :: Zoo -> String -> (Zoo, Zoo)
sortSpeciesWithSubstr zoo str = (filter (\(name, _count) -> hasSubstr name str) zoo, filter (\(name, _count) -> not (hasSubstr name str)) zoo)

-------------------------------------

{-

The rabbit population of the zoo increases every year. In year 0, there were 2 rabbits, while in year 1 there were 3 rabbits. In the following
years, the number of rabbits corresponds to the sum of the rabbit population of the two previous years.

-}

-- 8

{-

Implement rabbits :: (Integral a) => [a], which returns an infinite list with the rabbit population of each year (starting at year 0).
Use case example:
ghci> rabbits
[2,3,5,8 …]

-}

rabbitsAux :: (Integral a) => [a] -> [a]
rabbitsAux currList =  newSeq ++ rabbitsAux newList
    where reversedCurr = reverse currList
          newSeq = [(!!) reversedCurr 0 + (!!) reversedCurr 1]
          newList = currList ++ newSeq

rabbits :: (Integral a) => [a]
rabbits = [2,3] ++ rabbitsAux [2,3]


-- 9

{-

Implement rabbitYears :: (Integral a) => a -> Int, which returns the number of years needed for the rabbit population to be greater or equal
to the input integral value.

-}

rabbitYears :: (Integral a) => a -> Int
rabbitYears n = length (takeWhile (\x -> x < n) rabbits)


---------------------------

{-

Consider a dendrogram as a binary tree where each path leads to a string. Each non-leaf node of the dendrogram specifies the horizontal
distance from the father node to each of the two child nodes. A father node is always at an equal horizontal distance from both its children.

-}

data Dendrogram = Leaf String | Node Dendrogram Int Dendrogram


-- 10

{-

Implement dendroWidth :: Dendrogram -> Int, which returns the width of a dendrogram (i.e., the horizontal distance between the leftmost
and rightmost leaf nodes).
Use case example:
ghci> dendroWidth myDendro
13

-}

myDendro :: Dendrogram
myDendro = Node (Node (Leaf "dog") 3 (Leaf "cat")) 5 (Leaf "octopus")

dendroWidthEsq :: Dendrogram -> Int
dendroWidthEsq (Leaf _) = 0
dendroWidthEsq (Node esq n dir ) = n + dendroWidthEsq esq

dendroWidthDir :: Dendrogram -> Int
dendroWidthDir (Leaf _) = 0
dendroWidthDir (Node esq n dir ) = n + dendroWidthEsq dir

dendroWidth :: Dendrogram -> Int
dendroWidth (Leaf _) = 0
dendroWidth (Node esq n dir ) = dendroWidthEsq esq  + n*2 + dendroWidthDir dir
