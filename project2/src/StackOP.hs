module StackOP where 
import Auxx

add :: (Code, Stack, State) -> (Code, Stack, State) -- Add operation, adds the result to the top of the stack
add (Add:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Left (i1 + i2):tailstack, state)
add (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error"
add (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error"
add (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error" 
add (_, [el], _) = error "Run-time error"
add (_, _, _) = error "Run-time error"

mult :: (Code, Stack, State) -> (Code, Stack, State) -- Multiplication operation, adds the result to the top of the stack
mult (Mult:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Left (i1 * i2):tailstack, state)
mult (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error"
mult (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error"
mult (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error"
mult (_, [el], _) = error "Run-time error"
mult (_, _, _) = error "Run-time error"

sub :: (Code, Stack, State) -> (Code, Stack, State) -- Subtraction operation, adds the result to the top of the stack
sub (Sub:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Left (i1 - i2):tailstack, state)
sub (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error"
sub (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error"
sub (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error"
sub (_, [el], _) = error "Run-time error"
sub (_, _, _) = error "Run-time error"

equ :: (Code, Stack, State) -> (Code, Stack, State) -- Equality operation, adds the result to the top of the stack
equ (Equ:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Right( boolToStr(i1 == i2)):tailstack, state)
equ (Equ:tailcode, (Right i1):(Right i2):tailstack, state) = (tailcode, Right ( boolToStr( strToBool i1 == strToBool i2)):tailstack, state)
equ (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error"
equ (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error"
equ (_, [el], _) = error "Run-time error"
equ (_, _, _) = error "Run-time error"

le :: (Code, Stack, State) -> (Code, Stack, State) -- Less than or equal operation, adds the result to the top of the stack
le (Le:tailcode, (Left i1):(Left i2):tailstack, state) = (tailcode, Right( boolToStr(i1 <= i2)):tailstack, state)
le (_code, (Right _s1):(Left _i2):_tailstack, _state) = error "Run-time error"
le (_code, (Left _i1):(Right _s2):_tailstack, _state) = error "Run-time error"
le (_code, (Right _s1):(Right _s2):_tailstack, _state) = error "Run-time error"
le (_, [el], _) = error "Run-time error"
le (_, _, _) = error "Run-time error"

true :: (Code, Stack, State) -> (Code, Stack, State) -- Tru operation, adds boolean true value to the top of the stack
true (Tru:tailcode, stack, state) = (tailcode, Right "tt":stack, state)
true (_, _, _) = error "Run-time error"

false :: (Code, Stack, State) -> (Code, Stack, State) -- Fals operation, adds boolean true value to the top of the stack
false (Fals:tailcode, stack, state) = (tailcode, Right "ff":stack, state)
false (_, _, _) = error "Run-time error"

push :: (Code, Stack, State) -> (Code, Stack, State) -- Push operation, adds an integer value to the top of the stack
push (Push i:tailcode, stack, state) = (tailcode, Left i:stack, state)
push (_, _, _) = error "Run-time error"

store :: (Code, Stack, State) -> (Code, Stack, State) -- Store operation, adds a pair variable,value to the state by popping the top of the stack
store ((Store ""):_, _, _) = error "Run-time error"
store ((Store var):tailcode, el:tailstack, state) = (tailcode, tailstack, updateState var el state)
store (_, [], _) = error "Run-time error"
store (_, _, _) = error "Run-time error"

fetch :: (Code, Stack, State) -> (Code, Stack, State) -- Fetch operation, adds the value of a variable to the top of the stack
fetch ((Fetch ""):_, _, _) = error "Run-time error"
fetch ((Fetch var):tailcode, stack, state) = (tailcode, searchVar var state : stack, state)
fetch (_, _, []) = error "Run-time error"
fetch (_, _, _) = error "Run-time error"

noop :: (Code, Stack, State) -> (Code, Stack, State) -- Noop operation, does nothing
noop (Noop:tailcode, stack, state) = (tailcode, stack, state)
noop (_, _, _) = error "Run-time error"

branch :: (Code, Stack, State) -> (Code, Stack, State) -- Branch operation, pops the top of the stack and jumps to the corresponding code
branch ((Branch code1 cod2):tailcode, (Right "tt"):tailstack, state) = (code1 ++ tailcode, tailstack, state)
branch ((Branch code1 code2):tailcode, (Right "ff"):tailstack, state) = (code2 ++ tailcode, tailstack, state)
branch (_, (Right _):_, _) = error "Run-time error"
branch (_, _, _) = error "Run-time error"

loop :: (Code, Stack, State) -> (Code, Stack, State) -- Loop operation, used for whiles.
loop ((Loop code1 code2):tailcode, stack, state) = (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ tailcode, stack, state)
loop (_, _, _) = error "Run-time error"

neg :: (Code, Stack, State) -> (Code, Stack, State) -- Neg operation, negates the top of the stack
neg (Neg:tailcode, (Right boolval):tailstack, state) = (tailcode, Right (boolToStr (not (strToBool boolval))):tailstack, state)
neg (_, _, _) = error "Run-time error"  

andF :: (Code, Stack, State) -> (Code, Stack, State) -- And operation, does Logical AND on the top two elements of the stack
andF (And:tailcode, (Right boolval1):(Right boolval2):tailstack, state) = (tailcode, Right (boolToStr (strToBool boolval1 && strToBool boolval2)):tailstack, state)
andF (_, _, _) = error "Run-time error"

