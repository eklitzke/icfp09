module TeamCA.Machine.SType
    ( SType(SType)
    , SOper ( Noop
            , Cmpz
            , Sqrt
            , Copy
            , Input
            , End )
    ) where

import TeamCA.Machine.Types

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
    fromEnum Noop  = 1
    fromEnum Cmpz  = 2
    fromEnum Sqrt  = 3
    fromEnum Copy  = 4
    fromEnum Input = 5

    toEnum 0 = Noop
    toEnum 1 = Cmpz
    toEnum 2 = Sqrt
    toEnum 3 = Copy
    toEnum 4 = Input
    toEnum x = error $ " unexpected SOper " ++ (show x)

