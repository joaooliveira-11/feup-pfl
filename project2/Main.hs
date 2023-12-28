module Main where

import Aux
import StackOP
import Current


testCases :: [([Inst], (String, String))]
testCases = [
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

main :: IO ()
main = do
    putStrLn "Running tests..."
    results <- mapM runTest testCases
    let passed = length (filter id results)
    putStrLn $ show passed ++ " out of " ++ show (length testCases) ++ " tests passed."
    putStrLn "All tests completed."

runTest :: ([Inst], (String, String)) -> IO Bool
runTest (input, expected) = do
    let output = testAssembler input
    if output == expected
        then do
            putStrLn "Test passed."
            return True
        else do
            putStrLn $ "Test failed. Expected " ++ show expected ++ ", but got " ++ show output
            return False
