module Parser where
import Compiler
import Data.Char

import Aux
import Debug.Trace

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
parseIfThenElse :: [Token] -> Maybe (Stm, [Token])
{-
parseIfThenElse (IfToken : OpenPToken : tokens1) =
  case parseBexp tokens1 of
    Just (bexp, ClosePToken : ThenToken : OpenPToken : tokens2) ->
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
        (thenStms, ClosePToken : tokens3) ->
          case tokens3 of
            (ElseToken : OpenPToken : tokens4) ->
              case parseStms tokens4 of
                (elseStms, ClosePToken : SemiColonToken : restTokens) -> Just (IfThenElse bexp (Seq thenStms) (Seq elseStms), restTokens)
                _ -> Nothing
            (ElseToken : tokens4) ->
              case parseStm tokens4 of
                (elseStm, restTokens) -> Just (IfThenElse bexp (Seq thenStms) elseStm, restTokens)
    Just (bexp, ClosePToken : ThenToken : tokens2) ->
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
-}
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
        (thenStms, ClosePToken : tokens3) ->
          case tokens3 of
            (ElseToken : OpenPToken : tokens4) ->
              case parseStms tokens4 of
                (elseStms, ClosePToken : SemiColonToken : restTokens) -> Just (IfThenElse bexp (Seq thenStms) (Seq elseStms), restTokens)
                _ -> Nothing
            (ElseToken : tokens4) ->
              case parseStm tokens4 of
                (elseStm, restTokens) -> Just (IfThenElse bexp (Seq thenStms) elseStm, restTokens)
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
    Just (aexp, SemiColonToken : rest') -> (Assign var aexp, rest') --  trace ("Remaining tokens test1: " ++ show rest') (Assign var aexp, rest')
    _ -> error "Missing semicolon or invalid syntax in arithmetic expression"
parseStm tokens@(IfToken : _) =
  case parseIfThenElse tokens of
    Just (stm, restTokens) -> (stm, restTokens) -- trace ("Remaining tokens test2: " ++ show restTokens) (stm, restTokens)
    _ -> error "Invalid syntax in if then else statement"
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

