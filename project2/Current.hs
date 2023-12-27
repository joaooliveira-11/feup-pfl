module Current where 

import Data.List (intercalate, sortOn)
import Aux
import Modules

{-
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- (a) Define a new type to represent the machine's stack. The type must be named Stack.
type StackType = Either Int String
type Stack = [StackType]

-}

-- (c) Implement the createEmptyStack function which returns an empty machine's stack.
createEmptyStack :: Stack
createEmptyStack = []

{-
-- (b) Define a new type to represent the machine's state. The type must be named State.
type State = [(String, StackType)]
-}

-- (d) Implement the createEmptyState function which returns an empty machine's state.
createEmptyState :: State
createEmptyState = []

-- (e) Implement the stack2Str function which converts a stack given as input to a string
stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map stackElementToStr $ reverse stack

{-

input: stack2Str [Right "ff", Right "tt",Left 42]
output: "42,True,False"

-}


-- (f) Implement the state2Str function which converts a machine state given as input to a string
state2Str :: State -> String
state2Str state = intercalate "," $ map statePairToStr $ sortOn fst state

{-

*Current> state2Str [("var", Right "tt"), ("a" , Left 3), ("someVar", Right "ff")]
"a=3,someVar=False,var=True"

-}

-- (g) Implement the run function 
{-

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((inst:rest), stack, state) = case inst of
  Push n -> run (rest, Left n : stack, state)

-}





