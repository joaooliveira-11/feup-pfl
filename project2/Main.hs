module Main where

import Aux
import StackOP
import Current
import Compiler


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

main :: IO ()
main = do
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
            putStrLn "Test passed."
            return True
        else do
            putStrLn $ "Test failed. Expected " ++ show expected ++ ", but got " ++ show output
            return False

runTestParser :: (String, (String, String)) -> IO Bool
runTestParser (input, expected) = do
    let output = testParser input
    if output == expected
        then do
            putStrLn "Test passed."
            return True
        else do
            putStrLn $ "Test failed. Expected " ++ show expected ++ ", but got " ++ show output
            return False
