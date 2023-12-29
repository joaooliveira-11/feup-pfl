module Compiler where
import Aux

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
compB (BLe aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (BAnd bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [And]
compB (BNeg bexp) = compB bexp ++ [Neg]

testCompile :: IO Code
testCompile = do
    let prog1 = [Assign "x" (Number 5), Assign "y" (AAdd (Variable "x") (Number 3))] -- x :=5; y := x+3
    let prog2 = [Assign "y" (Number 1), Assign "x" (Number 5),   -- y := 1; x := 5; while ¬(x = 1) do (y := y ∗ x; x := x − 1)
                 While (BNeg (BEqu (Variable "x") (Number 1))) 
                       (Seq [Assign "y" (AMul (Variable "y") (Variable "x")), 
                             Assign "x" (ASub (Variable "x") (Number 1))])]
    let prog3 = [Assign "y" (Number 7),  --  y := 7; if y <= 5 then x := 0 else x := 10
                 IfThenElse (BLe (Variable "y") (Number 5)) 
                    (Assign "x" (Number 0)) 
                    (Assign "x" (Number 10))]
    let progSeq = [Seq [Assign "x" (Number 5), Assign "y" (Number 3), Assign "z" (Number 2)]] -- x := 5; y := 3; z := 2
    putStrLn "Compiling... :"
    print progSeq
    let code1 = compile progSeq
    putStrLn "Result machine code :"
    print code1
    return code1