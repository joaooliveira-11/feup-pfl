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

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map stackElementToStr stack

state2Str :: State -> String
state2Str state = intercalate "," $ map statePairToStr $ sortOn fst state

run :: (Code, Stack, State) -> (Code, Stack, State)
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

compile :: Program -> Code
compile [] = []
compile (s:stms) = compStm s ++ compile stms

compStm :: Stm -> Code
compStm (Assign str aexp) = compA aexp ++ [Store str]
compStm (Seq stms) = concatMap compStm stms
compStm (IfThenElse bexp stm1 stm2) = compB bexp ++ [Branch (compStm stm1) (compStm stm2)]
compStm (While bexp stm) = [Loop (compB bexp) (compStm stm)]
compStm Skip = [Noop]

compA :: Aexp -> Code
compA (Number n) = [Push n]
compA (Variable str) = [Fetch str]
compA (AAdd aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (ASub aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (AMul aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]

compB :: Bexp -> Code
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

lexer :: String -> [Token]
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
                    then error $ "Invalid variable name: " ++ varStr
                    else VarToken varStr : lexer restStr
  | otherwise = error $ "Unrecognized character: " ++ [chr]

-- Parse parenthesised expressions
parseParenOrBasicExpr :: [Token] -> Maybe (Aexp, [Token])
parseParenOrBasicExpr (IntToken n : tailTokens) = Just (Number n, tailTokens)
parseParenOrBasicExpr (VarToken var : tailTokens) = Just (Variable var, tailTokens)
parseParenOrBasicExpr (OpenPToken : tailTokens) = 
  case parseParentSumsOrMultOrBasicExpr tailTokens of
    Just (expr, (ClosePToken : tailTokens1)) ->
      Just (expr, tailTokens1)
    Just _ -> Nothing -- missing closing parenthesis
    Nothing -> Nothing
parseParenOrBasicExpr tokens = Nothing

-- Parse products or parenthesis expressions
parseParenMulOrBasicExpr :: [Token] -> Maybe (Aexp, [Token])
parseParenMulOrBasicExpr tokens =
  case parseParenOrBasicExpr tokens of
    Just (expr1, (MultToken : tailTokens1)) ->
      case parseParenMulOrBasicExpr tailTokens1 of
        Just (expr2, tailTokens2) ->
          Just (AMul expr1 expr2, tailTokens2)
        Nothing -> Nothing
    result -> result

-- Parse sums or products or parenthesised expressions
parseParentSumsOrMultOrBasicExpr :: [Token] -> Maybe (Aexp, [Token])
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

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp (OpenPToken : tokens1) = case parseNotOrBoolEqualityOrAnd tokens1 of
  Just (stm, ClosePToken : rest) -> Just (stm, rest)
  _ -> Nothing
parseBexp (TrueToken : xs) = Just (TrueExp, xs)
parseBexp (FalseToken : xs) = Just (FalseExp, xs)
parseBexp tokens = parseArithmeticComparison tokens

parseNotOrBoolEqualityOrAnd :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolEqualityOrAnd xs = case parseNotOrBoolEquality xs of
  Just (stm, AndToken : rest) -> case parseNotOrBoolEqualityOrAnd rest of  
    Just (stm2, rest2) -> Just (BAnd stm stm2, rest2)
    _ -> Nothing
  result -> result

parseNotOrBoolEquality:: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolEquality xs = case parseNot xs of
  Just (stm, BEqBoolToken : rest) -> case parseNotOrBoolEquality rest of
    Just (stm2, rest2) -> Just (BEquality stm stm2, rest2)
    _ -> Nothing
  result -> result

parseNot:: [Token] -> Maybe (Bexp, [Token])
parseNot (NotToken : xs) = case parseBexp xs of
  Just (stm, rest) -> Just (BNeg stm, rest)
  _ -> Nothing
parseNot xs = parseBexp xs

parseArithmeticComparison :: [Token] -> Maybe (Bexp, [Token])
parseArithmeticComparison tokens = case parseParentSumsOrMultOrBasicExpr tokens of
  Just (exp, BEqAritToken : remaining) -> case parseParentSumsOrMultOrBasicExpr remaining of
    Just (exp2, remaining2) -> Just (BEqu exp exp2, remaining2)
    _ -> Nothing
  Just (exp, BLeToken : remaining) -> case parseParentSumsOrMultOrBasicExpr remaining of
    Just(exp2, remaining2) -> Just (BLe exp exp2, remaining2)
    _ -> Nothing
  result -> Nothing

------------------------------------------------------------------------------------------------
  
parseWhileLoop :: [Token] -> Maybe (Stm, [Token])
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

parseIfThenElse :: [Token] -> Maybe (Stm, [Token])
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

parseStms :: [Token] -> ([Stm], [Token])
parseStms tokens = 
  case tokens of
    (ClosePToken : _) -> ([], tokens)
    _ -> case parseStm tokens of
            (stm, restTokens) -> 
              let (stms, finalTokens) = parseStms restTokens
              in (stm : stms, finalTokens)

parseStm :: [Token] -> (Stm, [Token])
parseStm (VarToken var : AssignToken : tokens) =
  case parseParentSumsOrMultOrBasicExpr  tokens of
    Just (aexp, SemiColonToken : rest') ->  (Assign var aexp, rest') --  trace ("Remaining tokens test1: " ++ show rest') (Assign var aexp, rest')
    _ -> error "Missing semicolon or invalid syntax in arithmetic expression"
parseStm tokens@(IfToken : _) =
  case parseIfThenElse tokens of
    Just (stm, restTokens) -> (stm, restTokens) -- trace ("Remaining tokens test2: " ++ show restTokens) (stm, restTokens)
    _ -> error "Invalid syntax in if then else statement"
parseStm tokens@(WhileToken : _) =
  case parseWhileLoop tokens of
    Just (stm, restTokens) -> (stm, restTokens)
    _ -> error "Invalid syntax in while loop"
parseStm _ = error "Invalid syntax"

---------------------------------------------------------------------------

parseTokens :: [Token] -> Program
parseTokens [] = []
parseTokens tokens = let (stm, tailtoken) = parseStm tokens
                     in stm : parseTokens tailtoken

parse :: String -> Program
parse input = 
  case lexer input of
    tokens ->
      case parseTokens tokens of
        [] -> error "Empty program"
        prog -> prog

printToken:: String -> IO ()
printToken input = do
  let tokens = lexer input
  putStrLn $ "Tokens: " ++ show tokens 





{- ***************************************************** -}
{- ********************* Testers *********************** -}
{- ***************************************************** -}

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

testAssemblerCases :: [([Inst], (String, String))]
testAssemblerCases = [
    ([Push 10,Push 4,Push 3,Sub,Mult], ("-10","")),
    ([Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"], ("","a=3,someVar=False,var=True")),
    ([Fals,Store "var",Fetch "var"], ("False","var=False")),
    ([Push (-20),Tru,Fals], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg], ("False,True,-20","")),
    ([Push (-20),Tru,Tru,Neg,Equ], ("False,-20","")),
    ([Push (-20),Push (-21), Le], ("True","")),
    ([Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"], ("","x=4")),
    ([Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]], ("","fact=3628800,i=1"))
    ]

testParserCases :: [(String, (String, String))]
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
    ("i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" , ("","fact=3628800,i=1"))
    ]

runTests :: IO ()
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
   
runTestAssembler :: ([Inst], (String, String)) -> IO Bool
runTestAssembler (input, expected) = do
    let output = testAssembler input
    if output == expected
        then do
            putStrLn $ "\ESC[32mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Passed.\ESC[0m"
            return True
        else do
            putStrLn $ "\ESC[31mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Failed. Got " ++ show output ++ "\ESC[0m"
            return False

runTestParser :: (String, (String, String)) -> IO Bool
runTestParser (input, expected) = do
    let output = testParser input
    if output == expected
        then do
            putStrLn $ "\ESC[32mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Passed.\ESC[0m"
            return True
        else do
            putStrLn $ "\ESC[31mComparing " ++ show input ++ " and " ++ show expected ++ ". Result: Test Failed. Got " ++ show output ++ "\ESC[0m"
            return False
