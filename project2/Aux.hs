module Aux where

-- (e) Implement the stack2Str function which converts a stack given as input to a string
stackElementToStr :: Either Int String -> String
stackElementToStr (Left integer) = show integer
stackElementToStr (Right string) 
    | string == "tt" = "True"
    | string == "ff" = "False"
    | otherwise = error "Run-time error"


statePairToStr :: (String, Either Int String) -> String
statePairToStr (var, value) = var ++ "=" ++ stackElementToStr value
