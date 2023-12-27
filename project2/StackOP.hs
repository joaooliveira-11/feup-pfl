module StackOP where 
import Modules
import Aux

add :: (Code, Stack, State) -> (Code, Stack, State)
add (Add:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Left (i1 + i2):tailstack, state)
add (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error" -- Error when using Add with string and integer
add (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Add with integer and string
add (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Add with two strings
add (_, [el], _) = error "Run-time error" -- Error when using Add with only one element in the Stack
add (_, _, _) = error "Run-time error" -- Other errors

mult :: (Code, Stack, State) -> (Code, Stack, State)
mult (Mult:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Left (i1 * i2):tailstack, state)
mult (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error" -- Error when using Mult with string and integer
mult (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Mult with integer and string
mult (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Mult with two strings
mult (_, [el], _) = error "Run-time error" -- Error when using Mult with only one element in the Stack
mult (_, _, _) = error "Run-time error" -- Other errors

sub :: (Code, Stack, State) -> (Code, Stack, State)
sub (Sub:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Left (i1 - i2):tailstack, state)
sub (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error" -- Error when using Mult with string and integer
sub (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Mult with integer and string
sub (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Mult with two strings
sub (_, [el], _) = error "Run-time error" -- Error when using Mult with only one element in the Stack
sub (_, _, _) = error "Run-time error" -- Other errors

equ :: (Code, Stack, State) -> (Code, Stack, State)
equ (Equ:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Right( boolToStr(i1 == i2)):tailstack, state)
equ (Equ:tailcode, (Right i1):(Right i2):tailstack, state) = (tailcode, Right ( boolToStr( strToBool i1 == strToBool i2)):tailstack, state)
equ (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error" -- Error when using Equ with string and integer
equ (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Equ with integer and string
equ (_, [el], _) = error "Run-time error" -- Error when using Equ with only one element in the Stack
equ (_, _, _) = error "Run-time error" -- Other errors

le :: (Code, Stack, State) -> (Code, Stack, State)
le (Le:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Right( boolToStr(i1 <= i2)):tailstack, state)
le (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error" -- Error when using Add with string and integer
le (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Add with integer and string
le (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error" -- Error when using Add with two strings
le (_, [el], _) = error "Run-time error" -- Error when using Add with only one element in the Stack
le (_, _, _) = error "Run-time error" -- Other errors

true :: (Code, Stack, State) -> (Code, Stack, State)
true (Tru:tailcode, stack, state) = (tailcode, Right "tt":stack, state)
true (_, _, _) = error "Run-time error"

false :: (Code, Stack, State) -> (Code, Stack, State)
false (Fals:tailcode, stack, state) = (tailcode, Right "ff":stack, state)
false (_, _, _) = error "Run-time error"

push :: (Code, Stack, State) -> (Code, Stack, State)
push (Push i:tailcode, stack, state) = (tailcode, Left i:stack, state)
push (_, _, _) = error "Run-time error"

store :: (Code, Stack, State) -> (Code, Stack, State)
store (Store "":_, _, _) = error "Run-time error"
store (Store var:tailcode, el:tailstack, state) = (tailcode, tailstack, updateState var el state)
store (_, [], _) = error "Run-time error"
store (_, _, _) = error "Run-time error"

fetch :: (Code, Stack, State) -> (Code, Stack, State)
fetch (Fetch "":_, _, _) = error "Run-time error"
fetch (Fetch var:tailcode, stack, state) = (tailcode, searchVar var state : stack, state)
fetch (_, _, []) = error "Run-time error"
fetch (_, _, _) = error "Run-time error"


{-

fetch :: (Code, Stack, State) -> (Code, Stack, State)
fetch (Fetch x:tailcode, stack, state) = case lookup x state of
    Just value -> (tailcode, value:stack, state)
    Nothing    -> error $ "Run-time error: variable " ++ x ++ " not found"
fetch (_, _, _) = error "Run-time error"


-}