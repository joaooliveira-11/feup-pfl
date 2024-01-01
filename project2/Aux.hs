module Aux where
import Data.Char
import Data.List

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type StackType = Either Integer String
type Stack = [StackType]
type State = [(String, StackType)]

data Aexp = Number Integer | Variable String | AAdd Aexp Aexp | ASub Aexp Aexp | AMul Aexp Aexp
   deriving (Show, Eq)

data Bexp = TrueExp | FalseExp | BEqu Aexp Aexp | BLe Aexp Aexp | BAnd Bexp Bexp | BNeg Bexp | BEquality Bexp Bexp
    deriving (Show, Eq)

data Stm = Assign String Aexp | Seq [Stm] | IfThenElse Bexp Stm Stm | While Bexp Stm | Skip
  deriving (Show, Eq)

type Program = [Stm]

data Token
  = PlusToken
  | MinusToken
  | MultToken
  | OpenPToken
  | ClosePToken
  | IntToken Integer
  | VarToken String
  | SemiColonToken
  | AssignToken
  | BLeToken
  | BEqAritToken
  | BEqBoolToken
  | IfToken
  | ThenToken
  | ElseToken
  | TrueToken
  | FalseToken
  | NotToken
  | AndToken
  | WhileToken
  | DoToken
  deriving (Show, Eq)

stackElementToStr :: StackType -> String
stackElementToStr (Left integer) = show integer
stackElementToStr (Right string) 
    | string == "tt" = "True"
    | string == "ff" = "False"
    | otherwise = error "Run-time error"

statePairToStr :: (String, StackType) -> String
statePairToStr (var, value) = var ++ "=" ++ stackElementToStr value

strToBool :: String -> Bool
strToBool "tt" = True
strToBool "ff" = False
strToBool _ = error "Run-time error"

boolToStr :: Bool -> String
boolToStr True = "tt"
boolToStr False = "ff"

updateState :: String -> StackType -> State -> State
updateState var el state = (var, el) : filter ((/= var) . fst) state

searchVar :: String -> State -> StackType
searchVar var state = case lookup var state of
    Just value -> value
    Nothing    -> error "Run-time error"

lexNumber :: String -> (String, String)
lexNumber = break (not . isDigit)

stringToInt :: String -> Integer
stringToInt = fromIntegral . foldl (\acc chr -> 10 * acc + digitToInt chr) 0

getReservedkeywords :: [String]
getReservedkeywords = ["not", "True", "False", "if", "then", "else", "and", "while", "do"]

isNotValidVar :: String -> Bool
isNotValidVar var = any (`isInfixOf` var) getReservedkeywords