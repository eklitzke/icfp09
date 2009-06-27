module TeamCA.Machine ( decodeWord ) where

import Data.Word

import TeamCA.Machine.DType
import TeamCA.Machine.SType
import TeamCA.Machine.Types
import TeamCA.Machine.Util
 
-- Stepping through a single instruction is a function like World -> IO
-- World. The new world has an incremented ProgramCounter, and shares the same
-- reference to an Instruction array and Memory array. Since the memory array is
-- mutable, the old world shouldn't be used anymore (since its memory contents
-- will reflect the new state of the world).

decodeWord :: Word32 -> Either SType DType
decodeWord w
    | oper == 0 = Left  $ SType (toEnum oper') (toEnum imm) lAddr'
    | otherwise = Right $ DType (toEnum oper) hAddr' lAddr'
    where
      oper = extractOper w
      (hAddr, lAddr) = extractLower w
      (oper', imm) = extractOpImm hAddr
      hAddr' = fromIntegral hAddr
      lAddr' = fromIntegral lAddr
