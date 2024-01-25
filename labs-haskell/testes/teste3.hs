import Text.Read (readMaybe)
{-

Consider a database of books. Each book has a title, a list of one or more authors and a description.

The following type declaration represents a single book:

-}

type Book = (String,[String],String)

myBooks :: [Book]
myBooks =
  [
  ("the da vinci code",["Dan Brown"],"a mystery thriller set after a murder in french soil"),
  ("the lost prologue",["Danilo Silverio","Dan Brown"],"a novel about how a wonderful language came to be"),
  ("war and peace",["Lev Tolstoy"],"a war story on the french invasion of russia")
  ]

-- 1

{-

Implement containsWord :: Book -> String -> Bool, which, given a book and a word, determines whether the book contains the word or not.

A book contains a given word if the exact word exists in the title and/or the description.

Assume that:

both the title and the description only use lowercase letters, without any punctuation (such as commas).
all words in the title and description are separated by exactly one space character.
Suggestion: use the "words" function from the Prelude (words :: String -> [String]) to split a text into a list of words.

Examples:

*Main> containsWord (head myBooks) "code"
True
*Main> containsWord (head myBooks) "cod"
False

-}

containsWord :: Book -> String -> Bool
containsWord (title, _, desc) str = (elem str titleList) || (elem str descList)
    where titleList = words title
          descList = words desc

containsWord1 :: Book -> String -> Bool
containsWord1 (title, _, desc) str = length ( filter (\x -> x == str) titleList ) >= 1 || length ( filter (\x -> x == str) descList ) >= 1
    where titleList = words title
          descList = words desc

containsWord3 :: Book -> String -> Bool
containsWord3 (title, _, desc) str = or (map (\x -> x == str) titlelist) || or (map (\y -> y == str) desclist) 
    where titlelist = words title
          desclist = words desc


-- 2

{-

Implement testBooks :: [Book] -> String -> String -> Bool, which, given a list of books and two words (a and b), determines whether all books that contain the word a also contain the word b.

Constraint: in this exercise, you must use list comprehensions. You are not allowed to use recursion or higher-order functions (map, filter, foldl ...).

Examples:

*Main> testBooks myBooks "french" "a"
True
*Main> testBooks myBooks "a" "french"
False
All books that contain "french" also contain "a", but not all books that contain "a" contain "french"

-}

testBooks :: [Book] -> String -> String -> Bool
testBooks books wa wb = and [containsWord book wb| book <- books, containsWord book wa]

testBooks1 :: [Book] -> String -> String -> Bool
testBooks1 books wa wb = booksWithA == booksWithAthenB
    where booksWithA = [book | book <- books, containsWord book wa]
          booksWithAthenB = [book | book <- booksWithA, containsWord book wb]

-- 3

myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse _ [el] = [el]
myIntersperse sep (h:t) = h:sep:myIntersperse sep t

-- 4

{-

Implement executiveSummaries :: [Book] -> Int -> [Book], which, given a list of books and an integer n, returns a new list of books where the description of each book is limited to its first n words.

The order of the resulting list must be the same as the input list.

A description remains unchanged if it has less than n words.

Constraint: in this exercise, you must use higher-order functions. You are not allowed to use recursion or list comprehensions.

Suggestion: use myIntersperse from the previous exercise, and the built-in functions map, words, take and concat.

Example:

*Main> executiveSummaries myBooks 3
[("the da vinci code",["Dan Brown"],"a mystery thriller"),
("the lost prologue",["Danilo Silverio","Dan Brown"],"a novel about"),
("war and peace",["Lev Tolstoy"],"a war story")]

-}

executiveSummaries :: [Book] -> Int -> [Book]
executiveSummaries books n = map(\(title, list, desc) -> (title, list, concat (myIntersperse " " desc))) limitedWords
    where limitedWords = map(\(title, list, desc) -> (title, list, take n (words desc))) books


-- using unwords for concat and intersparse with " "
executiveSummaries1 :: [Book] -> Int -> [Book]
executiveSummaries1 books n = map( \(title, list, desc) -> (title, list, unwords (take n (words desc)))) books

-- 5

{-

Implement longestBook :: [Book] -> String -> Maybe Book, which, given a list of books and an author name, returns a Maybe containing the book with the longest description among all books written by the author.

In the case of a tie, select the author's last longest book in the input list.

If the author did not write any of the books inside the input list, return a Nothing.

Example:

*Main> longestBook myBooks "Dan Brown"
Just ("the da vinci code",["Dan Brown"],"a mystery thriller set after a murder in french soil")
*Main> longestBook myBooks "Ehud Shapiro"
Nothing
Dan Brown's first book on myBooks has a longer description than the second book (52 vs 49 characters).

-}

longestBook :: [Book] -> String -> Maybe Book
longestBook books author
    | length authorbooks == 0 = Nothing
    | otherwise = Just ( head ( reverse ( filter (\(title, list, desc) -> (length desc) == maxLen) authorbooks ) ) )
    where 
        authorbooks = filter (\(_title, list, _desc) ->elem author list ) books
        maxLen = foldl (\acc (title, list, desc) -> max acc (length desc)) (-1) authorbooks


-- 9

{-

Implement squareMatrix :: Int -> [[Int]], which, given a positive integer n (n > 0), returns a n x n matrix with the numbers from 1 to n^2, in order.

Example:

*Main> squareMatrix 3
[[1,2,3],[4,5,6],[7,8,9]]

-}

squareMatrix :: Int -> [[Int]]
squareMatrix n = [ [lim..lim+n-1] | lim <- [1, 1+n..n^2]]


-- 10

{-

Implement aToB :: Int -> Int -> (Int -> Bool) -> [Int], which given a predicate p and two integers a and b, returns an ordered list with the a-th to b-th list of natural numbers (starting at 1) that satisfy p.

The integer indices start at 1.

Examples:

*Main> aToB 1 5 (\_ -> True)
[1,2,3,4,5]
*Main> aToB 2 4 odd
[3,5,7]

-}

aToB :: Int -> Int -> (Int -> Bool) -> [Int]
aToB a b p = [(!!) pList (idx-1) | idx <- [a..b]]
    where pList = [el | el <- [1..], p el]


-- 11

{-

Implement saddlePoints :: [[Int]] -> [(Int, Int)], which, given square matrix (i.e. same number of rows and columns) of integers, returns a list of the coordinates of its saddle points.

A saddle point is an element that is the minimum in its row and the maximum in its column.

Each coordinate has the format (row,column), with indices starting at 0.

The resulting list of saddle points must be ordered by row index, and, in case of a tie, by the column index.

Examples:

*Main> saddlePoints [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
[(3,0)]
*Main> saddlePoints [[0,1,0],[2,0,1],[1,0,1]]
[]
*Main> saddlePoints [[0,0,0],[2,0,1],[1,0,1]]
[(0,1),(1,1),(2,1)]

-}

saddlePoints :: [[Int]] -> [(Int, Int)]
saddlePoints matrix = [(r,c) | r <- [0..size], c <- [0..size], checkSaddle matrix r c]
    where size = length matrix -1

checkSaddle :: [[Int]] -> Int -> Int -> Bool
checkSaddle matrix r c = 
    currentSadle == rowmin && currentSadle == colmax
    where 
        currentSadle = matrix !! r !! c
        rowmin = minimum [el | el <- (matrix !! r)]
        colmax = maximum [row !! c | row <- matrix]


----------------------


{-

Consider a variant of your Haskell practical assignment, where the language you want to compile only has two types of statements: integer and boolean assignments.

An assignment has the following format (acording to the variable type):

int <variable_name> := <expression> ;
or

bool <variable_name> := <expression> ;
The expression can only be a constant or the name of another variable (i.e. no sums, products or logical negations, for example).

Assume that:

Variable names are a string of one or more lowercase characters.
There is always exactly one space between each symbol/token.
The language does not contain any parentheses, tabs or newlines.
Example of a source-code string:

"int a := 1 ; bool b := True ; bool cee := b ;"

-}

{-

Consider the type definitions below to represent, respectively, arithmetic expressions, boolean expressions, lines of code (statements) and programs. The function below parses arithmetic expressions.

import Text.Read (readMaybe)

data Aexp = IntegerVar String | IntegerConst Integer deriving Show
data Bexp = BooleanVar String | BooleanConst Bool deriving Show
data Stm = IntegerAssignment String Aexp | BooleanAssignment String Bexp deriving Show
type Program = [Stm]

parseAexp :: String -> Aexp
parseAexp value =
  case (readMaybe value)::Maybe Integer of Just number -> IntegerConst number
                                           Nothing -> IntegerVar value
You can copy the code excerpt above to your testing file. Do not modify this code and do not copy any part of it to the exam's answer boxes.

-}


data Aexp = IntegerVar String | IntegerConst Integer deriving Show
data Bexp = BooleanVar String | BooleanConst Bool deriving Show
data Stm = IntegerAssignment String Aexp | BooleanAssignment String Bexp deriving Show
type Program = [Stm]

parseAexp :: String -> Aexp
parseAexp value =
  case (readMaybe value)::Maybe Integer of Just number -> IntegerConst number
                                           Nothing -> IntegerVar value

-- 6

{-

Implement parseBexp :: String -> Bexp, which parses a string into a boolean expression, which can either be a constant value, True or False, or the name of a variable.

Assume that the input string is always a set of letters, without any spaces.

Hint: The structure of the function is very similar to parseAexp.

In this exercise only, you are allowed to use readMaybe from the Text.Read module. Do not call "import" in your code for this exercise.

-}


parseBexp  :: String -> Bexp
parseBexp  value =
  case (readMaybe value)::Maybe Bool of Just boolean -> BooleanConst boolean
                                        Nothing -> BooleanVar value

-- 7

{-


Using parseAexp and parseBexp implement parse :: String -> Program, which parses a source-code string into a program.

Assume that the input source-code string is always valid (no syntax or logic errors).

Note: the source-code string is composed only of integer and boolean assignments, as previously indicated.

Suggestion: Since you can assume that the symbols are all separated by a space, use the "words" function to obtain a list of symbols.

Example:

*Main> parse "int a := 1 ; bool b := True ; bool cee := b ;"
[IntegerAssignment "a" (IntegerConst 1),BooleanAssignment "b" (BooleanConst True),BooleanAssignment "cee" (BooleanVar "b")]

-}

parseAux :: [String] -> Program
parseAux [] = []
parseAux ("int":var:":=":val:";":tail) = IntegerAssignment var (parseAexp val) : (parseAux tail)
parseAux ("bool":var:":=":val:";":tail) = BooleanAssignment var (parseBexp val) : (parseAux tail)

parse :: String -> Program
parse str = parseAux wordslist
    where wordslist = words str


-- 8

{-

Consider the exact same instruction set of the Haskell practical assignment.

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


Implement compile :: Program -> Code, which converts a program into a set of machine instructions.

Assume that the input program is always correct (for instance, all variables are declared before they are used to define another variable).

-}

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

{-

data Aexp = IntegerVar String | IntegerConst Integer deriving Show  
data Bexp = BooleanVar String | BooleanConst Bool deriving Show
data Stm = IntegerAssignment String Aexp | BooleanAssignment String Bexp deriving Show
type Program = [Stm]

-}

compile :: Program -> Code
compile [] = []
compile (IntegerAssignment var aexp : t) = compileAexp aexp ++ [Store var] ++ compile t
compile (BooleanAssignment var bexp : t) = compileBexp bexp ++ [Store var] ++ compile t

compileAexp :: Aexp -> Code
compileAexp (IntegerVar var) = [Fetch var]
compileAexp (IntegerConst int) = [Push int]

compileBexp :: Bexp -> Code
compileBexp (BooleanVar var) = [Fetch var]
compileBexp (BooleanConst True) = [Tru]
compileBexp (BooleanConst False) = [Fals]