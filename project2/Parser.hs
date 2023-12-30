module Parser where
import Compiler
import Data.Char

import Aux

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
lexer ('i':'f':stringtail) = IfToken : lexer stringtail
lexer ('t':'h':'e':'n':stringtail) = ThenToken : lexer stringtail
lexer ('e':'l':'s':'e':stringtail) = ElseToken : lexer stringtail
lexer str@(chr : tailstring)
  | isSpace chr = lexer tailstring
  | isDigit chr = let (digitStr, restStr) = break (not . isDigit) str
                  in IntToken (stringToInt digitStr) : lexer restStr
  | isAlpha chr = let (varStr, restStr) = span isAlphaNum str
                  in VarToken varStr : lexer restStr
  | otherwise = error $ "Unrecognized character: " ++ [chr]

-- Parse integer literals or string variables
parseBasicExpr :: [Token] -> Maybe (Aexp, [Token])
parseBasicExpr (IntToken n : restTokens) = Just (Number n, restTokens)
parseBasicExpr (VarToken var : restTokens) = Just (Variable var, restTokens)
parseBasicExpr _ = Nothing

-- Parse products
parseMulExpr :: [Token] -> Maybe (Aexp, [Token])  
parseMulExpr tokens =
  case parseBasicExpr tokens of
    Just (expr1, (MultToken : restTokens1)) ->
      case parseMulExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AMul expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parse Sum and Subtraction
parseAddSubExpr :: [Token] -> Maybe (Aexp, [Token])
parseAddSubExpr tokens = 
  case parseMulExpr tokens of
    Just (expr1, (PlusToken : restTokens1)) ->
      case parseMulExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AAdd expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (MinusToken : restTokens1)) ->
      case parseMulExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ASub expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parse parenthesis expressions
parseParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseParenExpr (IntToken n : restTokens) = Just (Number n, restTokens)
parseParenExpr (VarToken var : restTokens) = Just (Variable var, restTokens)
parseParenExpr (OpenPToken : restTokens1) = 
  case parseAddSubParenExpr restTokens1 of
    Just (expr, (ClosePToken : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseParenExpr tokens = Nothing

-- Parse products or parenthesis expressions
parseMulParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseMulParenExpr tokens =
  case parseParenExpr tokens of
    Just (expr1, (MultToken : restTokens1)) ->
      case parseMulParenExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AMul expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

-- Parse sums or products or parenthesised expressions
parseAddSubParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseAddSubParenExpr tokens =
  case parseMulParenExpr tokens of
    Just (expr1, (PlusToken : restTokens1)) ->
      case parseAddSubParenExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AAdd expr1 expr2, restTokens2)
        _ -> Nothing
    Just (expr1, (MinusToken : restTokens1)) ->
      case parseAddSubParenExpr restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ASub expr1 expr2, restTokens2)
        _ -> Nothing
    result -> result


parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp (VarToken var : BLeToken : tokens) =
  case parseAddSubParenExpr tokens of
    Just (aexp, restTokens) -> Just (BLe (Variable var) aexp, restTokens)
    _ -> Nothing
parseBexp _ = Nothing

parseIfThenElse :: [Token] -> Maybe (Stm, [Token])
parseIfThenElse (IfToken : tokens1) =
  case parseBexp tokens1 of
    Just (bexp, ThenToken : tokens2) ->
      case parseStm tokens2 of
        (thenStm, ElseToken : tokens3) ->
          case parseStm tokens3 of
            (elseStm, restTokens) -> Just (IfThenElse bexp thenStm elseStm, restTokens)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
parseIfThenElse _ = Nothing


parseTokens :: [Token] -> Program
parseTokens [] = []
parseTokens tokens = let (stm, rest) = parseStm tokens
                     in stm : parseTokens rest

parseStm :: [Token] -> (Stm, [Token])
parseStm (VarToken var : AssignToken : tokens) =
  case parseAddSubParenExpr tokens of
    Just (aexp, SemiColonToken : rest') -> (Assign var aexp, rest')
    _ -> error "Missing semicolon or invalid syntax in arithmetic expression"
parseStm tokens@(IfToken : _) =
  case parseIfThenElse tokens of
    Just (stm, restTokens) -> (stm, restTokens)
    _ -> error "Invalid syntax in if then else statement"
parseStm _ = error "Invalid syntax"

parse :: String -> Program
parse str = 
  case lexer str of
    tokens ->
      case parseTokens tokens of
        [] -> error "Empty program"
        prog -> prog

printToken:: String -> IO ()
printToken input = do
  let tokens = lexer input
  putStrLn $ "Tokens: " ++ show tokens  -- Print the tokens after lexer 
