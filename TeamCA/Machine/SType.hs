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
 
data SType = SType SOper Imm Addr

-- The from/to enum is the opcode, as specified in the problem specification
instance Enum SOper where
    fromEnum Noop  = 1
    fromEnum Cmpz  = 2
    fromEnum Sqrt  = 3
    fromEnum Copy  = 4
    fromEnum Input = 5

    toEnum 1 = Noop
    toEnum 2 = Cmpz
    toEnum 3 = Sqrt
    toEnum 4 = Copy
    toEnum 5 = Input
