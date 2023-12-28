module Aux where

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type StackType = Either Integer String
type Stack = [StackType]

type State = [(String, StackType)]

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