module Current where 

import Data.List (intercalate, sortOn)
import Aux
import StackOP


createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," $ map stackElementToStr stack

state2Str :: State -> String
state2Str state = intercalate "," $ map statePairToStr $ sortOn fst state

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (Add:tailcode, stack, state) = run (add (Add:tailcode, stack, state))
run (Mult:tailcode, stack, state) = run (mult (Mult:tailcode, stack, state))
run (Sub:tailcode, stack, state) = run (sub (Sub:tailcode, stack, state))
run (Equ:tailcode, stack, state) = run (equ (Equ:tailcode, stack, state))
run (Le:tailcode, stack, state) = run (le (Le:tailcode, stack, state))
run (Tru:tailcode, stack, state) = run (true (Tru:tailcode, stack, state))
run (Fals:tailcode, stack, state) = run (false (Fals:tailcode, stack, state))
run ((Push i):tailcode, stack, state) = run (push ((Push i):tailcode, stack, state))
run ((Store var):tailcode, stack, state) = run (store ((Store var):tailcode, stack, state))
run ((Fetch var):tailcode, stack, state) = run (fetch ((Fetch var):tailcode, stack, state))
run (Noop:tailcode, stack, state) = run (noop (Noop:tailcode, stack, state))
run ((Branch code1 code2):tailcode, stack, state) = run (branch ((Branch code1 code2):tailcode, stack, state))
run ((Loop code1 code2):tailcode, stack, state) = run (loop ((Loop code1 code2):tailcode, stack, state))
run (Neg:tailcode, stack, state) = run (neg (Neg:tailcode, stack, state))
run (And:tailcode, stack, state) = run (andF (And:tailcode, stack, state))







