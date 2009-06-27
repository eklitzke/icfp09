module TeamCA.Machine
    ( World
    ) where

import Data.Array.IO
import Data.Word

import TeamCA.Machine.Util

-- An Addr is really 14 bits, but this should be close enough
type Addr = Word16
 
-- The status register
data StatusR = On | Off deriving (Show, Eq)
 
data Imm =  LTZ
          | LEZ
          | EQZ
          | GEZ
          | GTZ deriving (Show, Eq, Ord, Enum)
 
 
-- These are operations that require two source registers, as specified in Table
-- 1 of the problem specification
data DOper =  Add
            | Sub
            | Mult
            | Div
            | Output
            | Phi deriving (Show, Eq, Ord)

data SOper =  Noop
            | Cmpz
            | Sqrt
            | Copy
            | Input
 
data DType = DOper Addr Addr
 
data SType = SOper (Maybe Imm) Addr
 
-- The program counter
type ProgramCounter = Addr
 
-- The instruction set is immutable
type Instructions = IOArray Addr Word32
 
-- Memory is mutable
type Memory = IOArray Addr Word64
 
data World = World ProgramCounter StatusR Instructions Memory
 
-- Stepping through a single instruction is a function like World -> IO
-- World. The new world has an incremented ProgramCounter, and shares the same
-- reference to an Instruction array and Memory array. Since the memory array is
-- mutable, the old world shouldn't be used anymore (since its memory contents
-- will reflect the new state of the world).
