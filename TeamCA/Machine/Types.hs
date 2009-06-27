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
    , StatusR (On, Off)
    , World(World)
    , readPort
    , writePort
    ) where

import Prelude hiding (lookup)
import Data.Array.Unboxed
import Data.Array.IO
import Data.Map
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
type Instructions = UArray Addr Word32
 
-- Memory is mutable
type Memory = IOArray Addr Double

type Ports = Map Int Double

writePort :: Int -> Double -> Ports -> Ports
writePort = insert

readPort :: Int -> Ports -> Double
readPort k m = case lookup k m of
                 Nothing -> error ("failed to read from port " ++ (show k))
                 Just v -> v

-- |Create a new set of input ports with configuration value c.
newPorts :: Double -> Ports
newPorts c = fromList [(0x2, 0.0), (0x3, 0.0), (0x3e80, c)]

data World = World ProgramCounter StatusR Ports Instructions Memory
