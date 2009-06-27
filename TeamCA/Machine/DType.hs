module TeamCA.Machine.DType
    ( DType (DType)
    , DOper ( Add
            , Sub
            , Mult
            , Div
            , Output
            , Phi )
    ) where

import TeamCA.Machine.Types

-- These are operations that require two source registers, as specified in Table
-- 1 of the problem specification
data DOper =  Add
            | Sub
            | Mult
            | Div
            | Output
            | Phi deriving (Show, Eq, Ord)

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


data DType = DType DOper Addr Addr
