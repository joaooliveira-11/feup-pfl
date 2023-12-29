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

parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntToken n : restTokens) = Just (Number n, restTokens)
parseInt (VarToken var : restTokens) = Just (Variable var, restTokens)
parseInt _ = Nothing

parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (IntToken n : restTokens) = Just (Number n, restTokens)
parseIntOrParenExpr (VarToken var : restTokens) = Just (Variable var, restTokens)
parseIntOrParenExpr (OpenPToken : restTokens1) = 
  case parseSumOrProdOrIntOrPar restTokens1 of
    Just (expr, (ClosePToken : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing


parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])  
parseProdOrInt tokens =
  case parseInt tokens of
    Just (expr1, (MultToken : restTokens1)) ->
      case parseProdOrInt restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AMul expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrInt tokens = 
  case parseProdOrInt tokens of
    Just (expr1, (PlusToken : restTokens1)) ->
      case parseProdOrInt restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AAdd expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (MinusToken : restTokens1)) ->
      case parseProdOrInt restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ASub expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens =
  case parseIntOrParenExpr tokens of
    Just (expr1, (MultToken : restTokens1)) ->
      case parseProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AMul expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens =
  case parseProdOrIntOrPar tokens of
    Just (expr1, (PlusToken : restTokens1)) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (AAdd expr1 expr2, restTokens2)
        _ -> Nothing
    Just (expr1, (MinusToken : restTokens1)) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (ASub expr1 expr2, restTokens2)
        _ -> Nothing
    result -> result


parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp (VarToken var : BLeToken : tokens) =
  case parseSumOrProdOrIntOrPar tokens of
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
  case parseSumOrProdOrIntOrPar tokens of
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
