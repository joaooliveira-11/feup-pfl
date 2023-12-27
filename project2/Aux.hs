module Aux where
import Modules

-- (e) AUX FOR Implement the stack2Str function which converts a stack given as input to a string
stackElementToStr :: StackType -> String
stackElementToStr (Left integer) = show integer
stackElementToStr (Right string) 
    | string == "tt" = "True"
    | string == "ff" = "False"
    | otherwise = error "Run-time error"

-- (f) AUX FOR Implement the state2Str function which converts a machine state given as input to a string
statePairToStr :: (String, StackType) -> String
statePairToStr (var, value) = var ++ "=" ++ stackElementToStr value

-- Auxiliar functions for the StackOP.hs
strToBool :: String -> Bool
strToBool "tt" = True
strToBool "ff" = False
strToBool _ = error "Run-time error"

-- Auxiliar functions for the StackOP.hs
boolToStr :: Bool -> String
boolToStr True = "tt"
boolToStr False = "ff"

-- Auxiliar functions for the StackOP.hs
updateState :: String -> StackType -> State -> State
updateState var el state = (var, el) : filter ((/= var) . fst) state

-- Auxiliar functions for the StackOP.hs
searchVar :: String -> State -> StackType
searchVar var state = case lookup var state of
    Just value -> value
    Nothing    -> error "Run-time error"