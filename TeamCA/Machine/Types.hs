module TeamCA.Machine.Types
    (
      Addr
    , Imm ( LTZ
          , LEZ
          , EQZ
          , GEZ
          , GTZ )
    , Instruction
    , Instructions
    , Memory
    , ProgramCounter
    , StatusR (On, Off)
    , Ports
    , World(World)
    , newPorts
    , readPort
    , writePort
    , newMemory
    , mkInstructions
    , emptyPorts

    -- SType
    , SType(SType)
    , SOper ( Noop
            , Cmpz
            , Sqrt
            , Copy
            , Input
            , End )

    -- DType
    , DType (DType)
    , DOper ( Add
            , Sub
            , Mult
            , Div
            , Output
            , Phi )
    -- Solutions
    , Frame(..)
    , Solution(..)

    ) where

import Prelude hiding (lookup)
import Data.Array.Unboxed
import Data.Array.IO
import Data.Map hiding (elems)
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

type Instruction = Either SType DType

showInstruction :: Instruction -> String
showInstruction (Left s) = show s
showInstruction (Right d) = show d

-- The instruction set is immutable
type Instructions = Array Addr Instruction

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

emptyPorts :: Ports
emptyPorts = fromList []

-- Make an instruction array from a list of word32 instructions
mkInstructions :: [Instruction] -> Instructions
mkInstructions is = listArray (addrMin, addrMax) (is ++ [Left $ SType End undefined undefined])

-- The max address
addrMax :: Addr
addrMax = (2 ^ 14) - 1

-- The min address
addrMin :: Addr
addrMin = 0

-- Make memory from a list of doubles
newMemory :: [Double] -> IO Memory
newMemory doubles = newListArray (addrMin, addrMax) (doubles ++ repeat 0.0)

data World = World ProgramCounter StatusR Ports Instructions Memory

instance Show World where
    show (World pc sr ports is ms) = "World(pc=" ++ show pc ++ " ports=" ++ show (toList ports) ++ ")"


data SOper =  Noop
            | Cmpz
            | Sqrt
            | Copy
            | Input
            | End -- pseudo instruction, means remaining instructions are all
                  -- Noops; this won't appear in .obf files, it's just an
                  -- optimization.
    deriving (Ord, Eq, Show)

data SType = SType SOper Imm Addr
    deriving (Ord, Eq, Show)

-- The from/to enum is the opcode, as specified in the problem specification
instance Enum SOper where
    fromEnum Noop  = 0
    fromEnum Cmpz  = 1
    fromEnum Sqrt  = 2
    fromEnum Copy  = 3
    fromEnum Input = 4
    fromEnum End = 5

    toEnum 0 = Noop
    toEnum 1 = Cmpz
    toEnum 2 = Sqrt
    toEnum 3 = Copy
    toEnum 4 = Input
    toEnum 5 = End
    toEnum x = error $ " unexpected SOper " ++ (show x)

-- These are operations that require two source registers, as specified in Table
-- 1 of the problem specification
data DOper =  Add
            | Sub
            | Mult
            | Div
            | Output
            | Phi 
            deriving (Show, Eq, Ord)

-- The from/to enum is the opcode, as specified in the problem specification
instance Enum DOper where
    fromEnum Add    = 1
    fromEnum Sub    = 2
    fromEnum Mult   = 3
    fromEnum Div    = 4
    fromEnum Output = 5
    fromEnum Phi    = 6

    toEnum 1 = Add
    toEnum 2 = Sub
    toEnum 3 = Mult
    toEnum 4 = Div
    toEnum 5 = Output
    toEnum 6 = Phi

type TeamID = Int
type ScenarioID = Int
type TimeStep = Int

data Solution = Solution TeamID ScenarioID [Frame]
    deriving (Ord, Eq, Show)

data Frame = Frame TimeStep Ports
    deriving (Ord, Eq, Show)

data DType = DType DOper Addr Addr
    deriving (Ord, Eq, Show)
