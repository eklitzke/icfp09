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
    , emptyPorts
    , readPort
    , writePort
    , newMemory
    , mkInstructions
    , outputPorts
    , inputPorts
    , InputPorts
    , OutputPorts

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
    , showInstruction
    , showProgram

    ) where

import Prelude hiding (lookup)
import Data.Array.Unboxed
import Data.Array.IO
import Data.List (intersperse)
import Data.Map hiding (elems, map)
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
    toEnum x = error $ "unexpected " ++ (show x)

    fromEnum LTZ = 0
    fromEnum LEZ = 1
    fromEnum EQZ = 2
    fromEnum GEZ = 3
    fromEnum GTZ = 4

-- The program counter
type ProgramCounter = Addr

type Instruction = Either SType DType

showReg :: Integral a => a -> String
showReg x = "r[" ++ show x ++ "]"

showInstruction :: Instruction -> String
showInstruction (Left (SType Noop _ _)) = "NOOP"
showInstruction (Left (SType Cmpz imm r)) = "CMPZ " ++ show imm ++ showReg r
showInstruction (Left (SType Sqrt _ r)) = "SQRT " ++ showReg r
showInstruction (Left (SType Copy _ r)) = "COPY " ++ showReg r
showInstruction (Left (SType Input _ r)) = "INPUT " ++ showReg r
showInstruction (Left (SType End _ r)) = "END"
showInstruction (Right (DType i r1 r2)) = show i ++ " " ++ showReg r1 ++ " " ++ showReg r2

showProgram :: Instructions -> String
showProgram instrs = concat $ intersperse "\n" $ map showInstruction $ elems instrs

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
emptyPorts = Data.Map.empty

-- Make an instruction array from a list of word32 instructions
mkInstructions :: [Instruction] -> Instructions
mkInstructions is = listArray (addrMin, addrMax) (is ++ [Left $ SType End LTZ 0])

-- The max address
addrMax :: Addr
addrMax = (2 ^ 14) - 1

-- The min address
addrMin :: Addr
addrMin = 0

-- Make memory from a list of doubles
newMemory :: [Double] -> IO Memory
newMemory doubles = newListArray (addrMin, addrMax) (doubles ++ repeat 0.0)

type InputPorts = Ports

type OutputPorts = Ports

data World = World ProgramCounter StatusR InputPorts OutputPorts Instructions Memory

inputPorts (World _ _ ip _ _ _) = ip

outputPorts (World _ _ _ op _ _) = op

instance Show World where
    show (World pc sr iports oports is ms) = "World(pc=" ++ show pc ++ " iports=" ++ show (toList iports) ++ " oports=" ++ show (toList oports) ++ ")"

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

