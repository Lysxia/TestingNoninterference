{-# LANGUAGE FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}

module Instr where

import Data.Data
import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad

import ArbitraryF
import Labels
import Flags
import Observable

import LaTeX
import Util

{-------------------------------- Instructions --------------------------------}

-- The data in our machines
type Atom = Labeled Int

-- A very simple stack machine.  Unless otherwise indicated, instructions do not
-- alter memory and increment the PC by one.  No instruction alters the
-- instruction memory, and only Noop doesn't change the stack.  The stack has
-- two kinds of entries: data entries and return address entries.  No
-- instructions other than call and return can cross return address entries;
-- they are much the same as the bottom of a stack.  We notate return addresses
-- as pairs (retPC,retArgs); except as explicitly noted, stacks in the
-- documentation don't include return addresses, and notionally end right above
-- them.
data Instr =
  -- No-op --
    Noop
  
  -- Pure stack instructions --
  | Add
    -- Stack:  a b _ -> (a+b) _
  | Push Atom -- a :: Atom
    -- Stack:  _ -> a _
  | Pop
    -- Stack:  a _ -> _
  
  -- Memory instructions --
  | Load
    -- Stack:  addr _ -> mem[addr] _
  | Store
    -- Stack:  addr val _ -> _
    -- Memory: mem[addr] := val
  
  -- Basic control flow --
  | Jump
    -- Stack:  iaddr _ -> _
    -- PC:     pc := iaddr
  
  -- Function calls --
  | Call Int Bool -- number of args (A) and whether the function returns a value (R)
    -- Stack:  iaddr arg1 ... argA _ -> arg1 ... argA (pc,R)
    -- PC:     pc := iaddr
  | Return Bool -- Bool argument only so that we can introduce a bug
    -- Stack:  ret ...junk... (iaddr,True)  _ -> ret _
    --             ...junk... (iaddr,False) _ -> _
    -- PC:     pc := iaddr
  
  -- Halting
  | Halt
  
  deriving (Show, Eq, Read, Typeable, Data)

instance LaTeX Instr where
  toLaTeX (Push   a)   = "\\ii{Push}\\;" ++ toLaTeX a
  toLaTeX (Call   n r) = "\\ii{Call}\\CallArgs{" ++ toLaTeX n ++ "}\\CallRet{" ++ toLaTeX r ++ "}"
  toLaTeX (Return r)   = "\\ii{Return}\\ReturnRet{" ++ toLaTeX r ++ "}"
  toLaTeX i            = "\\ii{" ++ show i ++ "}"

-- CH: this is usually called Instruction Opcode!
data InstrKind =
    NOOP
  | ADD
  | SUB
  | PUSH
  | POP
  | LOAD
  | STORE
  | JUMP
  | JUMPNZ
  | CALL
  | RETURN
  | HALT
  | LABELOF
  deriving (Show, Eq, Read)

instance ArbitraryF Instr where
  arbitraryF = oneof $ [Push <$> labeled int,
                       return Pop,
                       return Add,
                       return Load,
                       return Store,
                       return Noop] ++
                      [return Jump | jumpAllowed gi] ++
                      (if callsAllowed gi then 
                         [liftM2 Call (choose (0,conf_max_call_args ?f))
                                      arbitrary,
                          liftM Return arbitrary]
                       else []) ++
                      [return Halt]
    where gi = gen_instrs ?f :: GenInstrs

  shrinkF Noop = []
  shrinkF i = 
    Noop :      -- Easiest way to shrink an instruction is replacing it with a Noop.
    case i of   -- Otherwise...
      Push x   -> map Push $ shrink x    
      Call a r -> Jump : map (uncurry Call) (shrink (a,r))
      Return True -> [Return False]
      _        -> []

instance Observable Instr where
  Push a ~~~ Push a' = a ~~~ a'
  i ~~~ i' = i == i'

  vary _ = error "Observable Instr implements no vary"
{- dead code:
  vary (Push a) = liftM Push (vary a)
  vary i = return i
-}

  shrinkV (Variation Noop Noop) = []
  shrinkV (Variation i i') = 
    (if shrink_to_noop ?f then (Variation Noop Noop :) else id) $
    case (i,i') of 
      (Push a,   Push a')    -> map (fmap Push) $ shrinkV (Variation a a')
      (Call a r, Call a' r') -> 
         Variation Jump Jump :
         map (fmap (uncurry Call)) (shrinkV (Variation (a,r) (a',r')))
      (Return True, Return True) ->
         [Variation (Return False) (Return False)]
      _                      -> []

