module TeamCA.Machine.Types
    (
      Addr
    , Imm ( LTZ
          , LEZ
          , EQZ
          , GEZ
          , GTZ )
    , Instructions
    , Memory
    , ProgramCounter
    , StatusR
    , World
    ) where

import Data.Array.IO
import Data.Word


-- The status register
data StatusR = On | Off deriving (Show, Eq)

-- An Addr is really 14 bits, but this should be close enough
type Addr = Word16

data Imm = LTZ
         | LEZ
         | EQZ
         | GEZ
         | GTZ deriving (Show, Eq, Ord)

instance Enum Imm where
    toEnum 0 = LTZ
    toEnum 1 = LEZ
    toEnum 2 = EQZ
    toEnum 3 = GEZ
    toEnum 4 = GTZ
    fromEnum LTZ = 0
    fromEnum LEZ = 1
    fromEnum EQZ = 2
    fromEnum GEZ = 3
    fromEnum GTZ = 4

-- The program counter
type ProgramCounter = Addr
 
-- The instruction set is immutable
type Instructions = IOArray Addr Word32
 
-- Memory is mutable
type Memory = IOArray Addr Word64
 
data World = World ProgramCounter StatusR Instructions Memory
