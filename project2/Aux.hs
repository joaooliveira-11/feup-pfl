module Aux where

-- (e) Implement the stack2Str function which converts a stack given as input to a string
elementToStr :: Either Int String -> String
elementToStr (Left integer) = show integer
elementToStr (Right string) = string