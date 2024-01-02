module Main where

import Aux
import StackOP

import Data.List (intercalate, sortOn)
import Data.Char
import Debug.Trace


main :: IO ()
main = do
  putStrLn "Running test for: PFL Second Project!"
  runTests



{- ***************************************************** -}
{- ******************** Assembler ********************** -}
{- ***************************************************** -}

createEmptyStack :: Stack -- Creates an empty stack
createEmptyStack = []

createEmptyState :: State -- Creates an empty state
createEmptyState = []

stack2Str :: Stack -> String -- Converts a Stack to a String
stack2Str stack = intercalate "," $ map stackElementToStr stack

state2Str :: State -> String -- Converts a State to a String
state2Str state = intercalate "," $ map statePairToStr $ sortOn fst state

run :: (Code, Stack, State) -> (Code, Stack, State) -- Runs the program code
run ([], stack, state) = ([], stack, state)
run (Add:tailcode, stack, state) = run (add (Add:tailcode, stack, state))
run (Mult:tailcode, stack, state) = run (mult (Mult:tailcode, stack, state))
run (Sub:tailcode, stack, state) = run (sub (Sub:tailcode, stack, state))
run (Equ:tailcode, stack, state) = run (equ (Equ:tailcode, stack, state))
run (Le:tailcode, stack, state) = run (le (Le:tailcode, stack, state))
run (Tru:tailcode, stack, state) = run (true (Tru:tailcode, stack, state))
run (Fals:tailcode, stack, state) = run (false (Fals:tailcode, stack, state))
run ((Push i):tailcode, stack, state) = run (push ((Push i):tailcode, stack, state))
run ((Store var):tailcode, stack, state) = run (store ((Store var):tailcode, stack, state))
run ((Fetch var):tailcode, stack, state) = run (fetch ((Fetch var):tailcode, stack, state))
run (Noop:tailcode, stack, state) = run (noop (Noop:tailcode, stack, state))
run ((Branch code1 code2):tailcode, stack, state) = run (branch ((Branch code1 code2):tailcode, stack, state))
run ((Loop code1 code2):tailcode, stack, state) = run (loop ((Loop code1 code2):tailcode, stack, state))
run (Neg:tailcode, stack, state) = run (neg (Neg:tailcode, stack, state))
run (And:tailcode, stack, state) = run (andF (And:tailcode, stack, state))





{- ***************************************************** -}
{- ******************** Compiler *********************** -}
{- ***************************************************** -}

compile :: Program -> Code -- Compiles a program
compile [] = []
compile (s:stms) = compStm s ++ compile stms

compStm :: Stm -> Code -- Compiles a statement
compStm (Assign str aexp) = compA aexp ++ [Store str]
compStm (Seq stms) = concatMap compStm stms
compStm (IfThenElse bexp stm1 stm2) = compB bexp ++ [Branch (compStm stm1) (compStm stm2)]
compStm (While bexp stm) = [Loop (compB bexp) (compStm stm)]
compStm Skip = [Noop]

compA :: Aexp -> Code -- Compiles an arithmetic expression
compA (Number n) = [Push n]
compA (Variable str) = [Fetch str]
compA (AAdd aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (ASub aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (AMul aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]

compB :: Bexp -> Code -- Compiles a boolean expression
compB TrueExp = [Tru]
compB FalseExp = [Fals]
compB (BEqu aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]
compB (BEquality bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]
compB (BLe aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (BAnd bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [And]
compB (BNeg bexp) = compB bexp ++ [Neg]





{- ***************************************************** -}
{- ********************* Parser ************************ -}
{- ***************************************************** -}

lexer :: String -> [Token] -- Converts a String to a list of Tokens, representing words.
lexer [] = []
lexer ('+':stringtail) = PlusToken : lexer stringtail
lexer ('-':stringtail) = MinusToken : lexer stringtail
lexer ('*':stringtail) = MultToken : lexer stringtail
lexer ('(':stringtail) = OpenPToken : lexer stringtail
lexer (')':stringtail) = ClosePToken : lexer stringtail
lexer (';':stringtail) = SemiColonToken : lexer stringtail
lexer (':':'=':stringtail) = AssignToken : lexer stringtail
lexer ('<':'=':stringtail) = BLeToken : lexer stringtail
lexer ('=':'=':stringtail) = BEqAritToken : lexer stringtail
lexer ('=':stringtail) = BEqBoolToken : lexer stringtail
lexer ('i':'f':stringtail) = IfToken : lexer stringtail
lexer ('t':'h':'e':'n':stringtail) = ThenToken : lexer stringtail
lexer ('e':'l':'s':'e':stringtail) = ElseToken : lexer stringtail
lexer ('T':'r':'u':'e':stringtail) = TrueToken : lexer stringtail
lexer ('F':'a':'l':'s':'e':stringtail) = FalseToken : lexer stringtail
lexer ('n':'o':'t':stringtail) = NotToken : lexer stringtail
lexer ('a':'n':'d':stringtail) = AndToken : lexer stringtail
lexer('w':'h':'i':'l':'e':stringtail) = WhileToken : lexer stringtail
lexer('d':'o':stringtail) = DoToken : lexer stringtail
lexer str@(chr : tailstring)
  | isSpace chr = lexer tailstring
  | isDigit chr = let (digitStr, restStr) = lexNumber str
                  in IntToken (stringToInt digitStr) : lexer restStr
  | isAlpha chr = let (varStr, restStr) = span isAlphaNum str
                  in if isNotValidVar varStr || not (isLower chr)
                    then error "Run-time error" -- error $ "Invalid variable name: " ++ varStr
                    else VarToken varStr : lexer restStr
  | otherwise = error "Run-time error" -- error $ "Unrecognized character: " ++ [chr]

parseParenOrBasicExpr :: [Token] -> Maybe (Aexp, [Token]) -- Parse parenthesised expressions
parseParenOrBasicExpr (IntToken n : tailTokens) = Just (Number n, tailTokens)
parseParenOrBasicExpr (VarToken var : tailTokens) = Just (Variable var, tailTokens)
parseParenOrBasicExpr (OpenPToken : tailTokens) = 
  case parseParentSumsOrMultOrBasicExpr tailTokens of
    Just (expr, (ClosePToken : tailTokens1)) ->
      Just (expr, tailTokens1)
    Just _ -> Nothing -- missing closing parenthesis
    Nothing -> Nothing
parseParenOrBasicExpr tokens = Nothing

parseParenMulOrBasicExpr :: [Token] -> Maybe (Aexp, [Token]) -- Parse products or parenthesis expressions
parseParenMulOrBasicExpr tokens =
  case parseParenOrBasicExpr tokens of
    Just (expr1, (MultToken : tailTokens1)) ->
      case parseParenMulOrBasicExpr tailTokens1 of
        Just (expr2, tailTokens2) ->
          Just (AMul expr1 expr2, tailTokens2)
        Nothing -> Nothing
    result -> result

parseParentSumsOrMultOrBasicExpr :: [Token] -> Maybe (Aexp, [Token]) -- Parse sums or products or parenthesised expressions
parseParentSumsOrMultOrBasicExpr tokens =
  case parseParenMulOrBasicExpr tokens of
    Just (expr1, (PlusToken : restTokens1)) ->
      case parseParentSumsOrMultOrBasicExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AAdd expr1 expr2, restTokens2)
        _ -> Nothing
    Just (expr1, (MinusToken : restTokens1)) ->
      case parseParentSumsOrMultOrBasicExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ASub expr1 expr2, restTokens2)
        _ -> Nothing
    result -> result

--------------------------------------------------------------------------------------


parseIntegerLessThanOrEqual :: Aexp -> [Token] -> Maybe (Bexp, [Token]) -- Parse integer less than or equal
parseIntegerLessThanOrEqual exp remaining = 
  case parseParentSumsOrMultOrBasicExpr remaining of
    Just(exp2, remaining2) -> Just (BLe exp exp2, remaining2)
    _ -> Nothing

parseIntegerEquality :: Aexp -> [Token] -> Maybe (Bexp, [Token]) -- Parse integer equality
parseIntegerEquality exp remaining = 
  case parseParentSumsOrMultOrBasicExpr remaining of
    Just (exp2, remaining2) -> Just (BEqu exp exp2, remaining2)
    _ -> Nothing

parseArithmeticComparison :: [Token] -> Maybe (Bexp, [Token]) -- Parse arithmetic comparison
parseArithmeticComparison tokens = 
  case parseParentSumsOrMultOrBasicExpr tokens of
    Just (exp, BLeToken : remaining) -> parseIntegerLessThanOrEqual exp remaining
    Just (exp, BEqAritToken : remaining) -> parseIntegerEquality exp remaining
    _ -> Nothing

parseNot:: [Token] -> Maybe (Bexp, [Token]) -- Parse not tokens
parseNot (NotToken : xs) = 
  case parseBexp xs of
    Just (stm, rest) -> Just (BNeg stm, rest)
    _ -> Nothing
parseNot xs = parseBexp xs

parseNotOrBoolEquality:: [Token] -> Maybe (Bexp, [Token]) -- Parse not or boolean equality tokens
parseNotOrBoolEquality xs = 
  case parseNot xs of
    Just (stm, BEqBoolToken : rest) -> case parseNotOrBoolEquality rest of
      Just (stm2, rest2) -> Just (BEquality stm stm2, rest2)
      _ -> Nothing
    result -> result

parseNotOrBoolEqualityOrAnd :: [Token] -> Maybe (Bexp, [Token]) -- Parse not or boolean equality or and tokens
parseNotOrBoolEqualityOrAnd xs = 
  case parseNotOrBoolEquality xs of
    Just (stm, AndToken : rest) -> case parseNotOrBoolEqualityOrAnd rest of  
      Just (stm2, rest2) -> Just (BAnd stm stm2, rest2)
      _ -> Nothing
    result -> result

parseBexp :: [Token] -> Maybe (Bexp, [Token]) -- Parse boolean expressions
parseBexp (OpenPToken : tokens1) = 
  case parseNotOrBoolEqualityOrAnd tokens1 of
    Just (stm, ClosePToken : rest) -> Just (stm, rest)
    _ -> Nothing
parseBexp (TrueToken : xs) = Just (TrueExp, xs)
parseBexp (FalseToken : xs) = Just (FalseExp, xs)
parseBexp tokens = parseArithmeticComparison tokens


------------------------------------------------------------------------------------------------
  
parseWhileLoop :: [Token] -> Maybe (Stm, [Token]) -- Parse while loops
parseWhileLoop (WhileToken : tokens1) =
  case parseBexp tokens1 of 
    Just (bexp, DoToken : tokens2) ->
      case tokens2 of
        (OpenPToken : tokens3) ->
          case parseStms tokens3 of
            (doStms, ClosePToken : SemiColonToken : tokens4) -> Just (While bexp (Seq doStms), tokens4)
            _ -> Nothing
        _ ->
          case parseStm tokens2 of
            (doStm, tokens3) -> Just (While bexp doStm, tokens3)
    _ -> Nothing
parseWhileLoop _ = Nothing

parseIfThenElse :: [Token] -> Maybe (Stm, [Token]) -- Parse if-then-else statements
parseIfThenElse (IfToken : tokens1) =
  case parseBexp tokens1 of
    Just (bexp, ThenToken : OpenPToken : tokens2) ->
      case parseStms tokens2 of
        (thenStms, ClosePToken : SemiColonToken : tokens3) ->
          case tokens3 of
            (ElseToken : OpenPToken : tokens4) ->
              case parseStms tokens4 of
                (elseStms, ClosePToken : SemiColonToken : restTokens) -> Just (IfThenElse bexp (Seq thenStms) (Seq elseStms), restTokens)
                _ -> Nothing
            (ElseToken : tokens4) ->
              case parseStm tokens4 of
                (elseStm, restTokens) -> Just (IfThenElse bexp (Seq thenStms) elseStm, restTokens)
            _ -> Nothing
        (thenStms, ClosePToken : tokens3) ->
          case tokens3 of
            (ElseToken : OpenPToken : tokens4) ->
              case parseStms tokens4 of
                (elseStms, ClosePToken : SemiColonToken : restTokens) -> Just (IfThenElse bexp (Seq thenStms) (Seq elseStms), restTokens)
                _ -> Nothing
            (ElseToken : tokens4) ->
              case parseStm tokens4 of
                (elseStm, restTokens) -> Just (IfThenElse bexp (Seq thenStms) elseStm, restTokens)
            _ -> Nothing
        _ -> Nothing
    Just (bexp, ThenToken : tokens2) ->
      case parseStm tokens2 of
        (thenStm, ElseToken : OpenPToken : tokens3) ->
          case parseStms tokens3 of
            (elseStms, ClosePToken : SemiColonToken : restTokens) -> Just (IfThenElse bexp thenStm (Seq elseStms), restTokens)
            _ -> Nothing
        (thenStm, ElseToken : tokens3) ->
          case parseStm tokens3 of
            (elseStm, restTokens) -> Just (IfThenElse bexp thenStm elseStm, restTokens)
            --_ -> Nothing
        _ -> Nothing
    _ -> Nothing
parseIfThenElse _ = Nothing

------------------------------------------------------------------

parseStms :: [Token] -> ([Stm], [Token]) -- Parse multiple statements. For example multiple nested stms in if-then-else and while loops.
parseStms tokens = 
  case tokens of
    (ClosePToken : _) -> ([], tokens)
    _ -> case parseStm tokens of
            (stm, restTokens) -> 
              let (stms, finalTokens) = parseStms restTokens
              in (stm : stms, finalTokens)

parseStm :: [Token] -> (Stm, [Token]) -- Parse a single statement
parseStm (VarToken var : AssignToken : tokens) =
  case parseParentSumsOrMultOrBasicExpr  tokens of
    Just (aexp, SemiColonToken : rest') ->  (Assign var aexp, rest') --  trace ("Remaining tokens test1: " ++ show rest') (Assign var aexp, rest')
    _ -> error "Run-time error"-- error "Missing semicolon or invalid syntax in arithmetic expression"
parseStm tokens@(IfToken : _) =
  case parseIfThenElse tokens of
    Just (stm, restTokens) -> (stm, restTokens) -- trace ("Remaining tokens test2: " ++ show restTokens) (stm, restTokens)
    _ -> error "Run-time error" -- error "Invalid syntax in if then else statement"
parseStm tokens@(WhileToken : _) =
  case parseWhileLoop tokens of
    Just (stm, restTokens) -> (stm, restTokens)
    _ -> error "Run-time error" -- error "Invalid syntax in while loop"
parseStm _ = error "Run-time error" -- error "Invalid syntax"

---------------------------------------------------------------------------

parseTokens :: [Token] -> Program -- Parse a list of tokens
parseTokens [] = []
parseTokens tokens = let (stm, tailtoken) = parseStm tokens
                     in stm : parseTokens tailtoken

parse :: String -> Program -- Parse a string
parse input = 
  case lexer input of
    tokens ->
      case parseTokens tokens of
        [] -> error "Run-time error" -- error "Empty program"
        prog -> prog

printToken:: String -> IO () -- Prints the tokens of a string after using the lexer function
printToken input = do
  let tokens = lexer input
  putStrLn $ "Tokens: " ++ show tokens 





{- ***************************************************** -}
{- ********************* Testers *********************** -}
{- ***************************************************** -}

testAssembler :: Code -> (String, String) -- Tests the assembler
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String) -- Tests the parser
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

testAssemblerCases :: [([Inst], (String, String))] -- Test cases for the assembler
testAssemblerCases = [
    ([Push 10,Push 4,Push 3,Sub,Mult], ("-10","")),
    ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True")),
    ([Fals,Store "var",Fetch "var"], ("False","var=False")),
    ([Push (-20),Tru,Fals], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20","")),
    ([Push (-20),Push (-21), Le], ("True","")),
    ([Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"], ("","x=4")),
    ([Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]], ("","fact=3628800,i=1")),
    
    ------------------------------------------------------------------------
    ---------------------------- Personal Tests ----------------------------
    ------------------------------------------------------------------------

    ([Push 5, Push 3, Add], ("8","")),
    ([Push 5, Push 3, Mult], ("15","")),
    ([Push 5, Push 3, Sub], ("-2","")),
    ([Push 5, Push 3, Le], ("True","")),
    ([Push 3, Push 5, Le], ("False","")),
    ([Push 5, Push 5, Le], ("True","")),
    ([Push 5, Push 5, Equ], ("True","")),
    ([Push 5, Push 3, Equ], ("False","")),
    ([Tru, Fals, Equ], ("False","")),
    ([Tru, Tru, Equ], ("True","")),
    ([Push 5, Store "x", Fetch "x"], ("5","x=5")), 
    ([Push 5, Push 3, Equ, Branch [Push 1] [Push 2]], ("2","")),
    ([Push 5, Push 3, Le, Branch [Push 1] [Push 2]], ("1","")),
    ([Push 3, Push 5, Le, Branch [Push 1] [Push 2]], ("2","")), 
    ([Push 10, Store "i", Push 1, Store "sum", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "sum", Add, Store "sum", Push 1, Fetch "i", Sub, Store "i"]], ("","i=1,sum=55"))

    ]

testParserCases :: [(String, (String, String))] -- Test cases for the parser
testParserCases = [
    ("x := 5; x := x - 1;" , ("","x=4")),
    ("x := 0 - 2;" , ("","x=-2")),
    ("if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" , ("","y=2")),
    ("x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" , ("","x=1")),

    ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" , ("","x=2")),
    ("x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" , ("","x=2,z=4")),
    ("x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" , ("","x=34,y=68")),
    ("x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" , ("","x=34")),

    ("if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" , ("","x=1")),
    ("if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" , ("","x=2")),
    ("x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" , ("","x=2,y=-10,z=6")),
    ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" , ("","fact=3628800,i=1")),



    ------------------------------------------------------------------------
    ---------------------------- Personal Tests ----------------------------
    ------------------------------------------------------------------------

    ("y := 10; z := y - 5;", ("", "y=10,z=5")),
    ("a := 3; b := a * 3; c := b - a;", ("", "a=3,b=9,c=6")),
    ("if True then a := 5; else a := 10;", ("", "a=5")),
    ("if False then b := 1; else b := 2;", ("", "b=2")),
    ("if (5 == 4+1) then x := 7; else x := 8;", ("", "x=7")),
    ("a := 4 * (3 + 2) - 10;", ("", "a=10")),
    ("b := (2 * 2) - (3 - 1);", ("", "b=2")),
    ("counter := 5; sum := 0; while (not(counter == 0)) do (sum := sum + counter; counter := counter - 1;);", ("", "counter=0,sum=15")),
    ("n := 4; factorial := 1; while (not(n == 1)) do (factorial := factorial * n; n := n - 1;);", ("", "factorial=24,n=1")),
    ("if (1 == 1) then x := 100; else x := 200;", ("", "x=100")),
    ("x := 5; if x == 5 then x := x * 2; else x := x + 2;", ("", "x=10")),
    ("y := 0; if (y == 0) then y := 9; else y := 1;", ("", "y=9")),
    ("if (2 <= 3) then a := 4; else a := 5; if (a == 4) then b := a * 2; else b := a + 2;", ("", "a=4,b=8")),
    ("x := 3; while (not(x == 0)) do (x := x - 1;);", ("", "x=0")),
    ("counter := 3; total := 0; while (not(counter == 0)) do (total := total + 5; counter := counter - 1;);", ("", "counter=0,total=15")),
    ("n := 5; sum := 0; while (not(n == 0)) do (sum := sum + n; n := n - 1;);", ("", "n=0,sum=15")),
    ("if (10 == 10) then a := 1; else a := 2; while (not(a == 5)) do (a := a + 1;);", ("", "a=5")),
    ("count := 1; sum := 0; while (count <= 5) do (sum := sum + count; count := count + 1;);", ("", "count=6,sum=15")),
    ("num := 4; product := 1; while (num <= 1) do (product := product * num; num := num - 1;);", ("", "num=4,product=1")),
    ("i := 1; total := 0; while (i <= 4) do (total := total + i * 2; i := i + 1;);", ("", "i=5,total=20")),
    ("x := 5; while (x <= 0) do (x := x - 1;);", ("", "x=5")),
    ("counter := 2; result := 0; while (counter <= 4) do (result := result + counter; counter := counter + 1;);", ("", "counter=5,result=9")),
    ("if (3 <= 4) then a := 2; else a := 3;", ("", "a=2")),
    ("a := 5; b := 10; if (b <= a) then c := a + b; else c := a - b;", ("", "a=5,b=10,c=-5")),
    ("x := 10; while (x <= 15) do (x := x + 1;);", ("", "x=16")),
    ("count := 1; total := 0; while (count <= 3) do (total := total + 2; count := count + 1;);", ("", "count=4,total=6")),
    ("if (1 <= 1) then x := 100; else x := 200;", ("", "x=100")),
    ("y := 5; if (y <= 5) then y := y * 2; else y := y + 2;", ("", "y=10")),
    ("z := 7; if (z <= 6) then z := 1; else z := 2;", ("", "z=2")),
    ("n := 3; sum := 0; while (n <= 5) do (sum := sum + n; n := n + 1;);", ("", "n=6,sum=12")),
    ("if (4 <= 3) then a := 4; else a := 5; if (a <= 5) then b := a * 2; else b := a + 2;", ("", "a=5,b=10")),
    ("x := 1; while (x <= 10) do (x := x + 2;);", ("", "x=11"))
    ]

runTests :: IO () -- Runs all the tests
runTests = do
    putStrLn "\n Running tests Assembler ..."
    results <- mapM runTestAssembler testAssemblerCases
    let passed = length (filter id results)
    putStrLn $ show passed ++ " out of " ++ show (length testAssemblerCases) ++ " tests passed."
    putStrLn "All tests completed."
    putStrLn "..............\n"
    
    putStrLn "\n Running tests Parser ..."
    results1 <- mapM runTestParser testParserCases
    let passed1 = length (filter id results1)
    putStrLn $ show passed1 ++ " out of " ++ show (length testParserCases) ++ " tests passed."
    putStrLn "All tests completed."
    putStrLn "..............\n"
   
runTestAssembler :: ([Inst], (String, String)) -> IO Bool -- Runs assembler tests
runTestAssembler (input, expected) = do
    let output = testAssembler input
    if output == expected
        then do
            putStrLn $ "\ESC[32mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Passed.\ESC[0m"
            return True
        else do
            putStrLn $ "\ESC[31mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Failed. Got " ++ show output ++ "\ESC[0m"
            return False

runTestParser :: (String, (String, String)) -> IO Bool -- Runs parser tests
runTestParser (input, expected) = do
    let output = testParser input
    if output == expected
        then do
            putStrLn $ "\ESC[32mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Passed.\ESC[0m"
            return True
        else do
            putStrLn $ "\ESC[31mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Failed. Got " ++ show output ++ "\ESC[0m"
            return False
