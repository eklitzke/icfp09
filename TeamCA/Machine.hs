module TeamCA.Machine ( step ) where

import Data.Word
import Data.IORef

import Data.Array.Unboxed
import Data.Array.IO

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

-- Read an instruction from a specified address
readText :: Instructions -> Addr -> Either SType DType
readText is addr = decodeWord (is ! addr)

readData :: Memory -> Addr -> IO Double
readData is addr = readArray is addr

step :: World -> IO World
step (World pc sr is ms) = do
  let instr = readText is pc
  sr' <- newIORef sr
  case instr of
    Left (SType Noop _ _) -> return ()
    Left (SType Cmpz imm r) -> do v <- readData ms r
                                  let sr'' = case imm of
                                           LTZ -> r < 0
                                           LEZ -> r <= 0
                                           EQZ -> r == 0
                                           GEZ -> r >= 0
                                           GTZ -> r > 0
                                  writeIORef sr' (if sr'' then On else Off)
    Left (SType Sqrt _ r) -> do v <- readData ms r
                                writeData (sqrt v)
    Left (SType Copy _ r) -> do v <- readData ms r
                                writeData v
    Left (SType Input _ r) -> error "Input not implemented"

    Right (DType Add  r1 r2) -> rHelper r1 r2 (+)
    Right (DType Sub  r1 r2) -> rHelper r1 r2 (-)
    Right (DType Mult r1 r2) -> rHelper r1 r2 (*)
    Right (DType Div  r1 r2) -> rHelper r1 r2 (/)
    Right (DType Output _ _) -> error "Output not implemented"
    Right (DType Phi  r1 r2) -> do v <- (case sr of
                                       On  -> readData ms r1
                                       Off -> readData ms r2)
                                   writeData v
  srVal <- readIORef sr'
  return $ World (pc + 1) srVal is ms
    where
      rHelper r1 r2 mut = do v1 <- readData ms r1
                             v2 <- readData ms r2
                             writeData (mut v1 v2)
      writeData v = writeArray ms sr v
