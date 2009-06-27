module TeamCA.Machine (
      step
    , readWorld
    , runWorld
    , updateWorld
    ) where


import Data.Array.Unboxed
import Data.Array.IO
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString
import Data.ByteString.Lazy (fromChunks)
import Data.IORef
import Data.Word
import System.IO
import TeamCA.Machine.Codec
import TeamCA.Machine.DType
import TeamCA.Machine.SType
import TeamCA.Machine.Types
import TeamCA.Machine.Util

-- Stepping through a single instruction is a function like World -> IO
-- World. The new world has an incremented ProgramCounter, and shares the same
-- reference to an Instruction array and Memory array. Since the memory array is
-- mutable, the old world shouldn't be used anymore (since its memory contents
-- will reflect the new state of the world).

-- Read an instruction from a specified address
readText :: Instructions -> Addr -> Either SType DType
readText is addr = is ! addr

readData :: Memory -> Addr -> IO Double
readData is addr = readArray is addr

-- |Step through an instruction of the world, creating a new world. If the new
-- world has a pc (program counter) value of 0, then this indicates that the
-- simulation has finished, and the caller should handle this appropriately
-- (i.e. by reading from the output ports, writing to the input ports, etc. as
-- necessary).
step :: World -> IO World
step (World pc sr ports is ms) = do
  let instr = readText is pc
  --putStrLn $ "step: instr = " ++ (show instr) ++ " pc = " ++ (show pc)
  sr' <- newIORef sr
  ports' <- newIORef ports
  pc' <- newIORef (pc + 1)
  case instr of
    Left (SType Noop _ _)   -> return ()
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
    Left (SType Input _ r) -> do let v = readPort (round $ fromIntegral r) ports
                                 writeData v
    Left (SType End _ _) -> do writeIORef pc' 0
                               writeIORef sr' Off
    Right (DType Add  r1 r2) -> rHelper r1 r2 (+)
    Right (DType Sub  r1 r2) -> rHelper r1 r2 (-)
    Right (DType Mult r1 r2) -> rHelper r1 r2 (*)
    Right (DType Div  r1 r2) -> rHelper r1 r2 (/)
    Right (DType Output r1 r2) -> do v <- readData ms r2
                                     let r1' = fromIntegral r1
                                     writeIORef ports' (writePort r1' v ports)
                                     return ()
    Right (DType Phi  r1 r2) -> do val <- case sr of
                                            On  -> readData ms r1
                                            Off -> readData ms r2
                                   writeData val

  srVal <- readIORef sr'
  portsVal <- readIORef ports'
  pcVal <- readIORef pc'
  return $ World pcVal srVal portsVal is ms
    where
      rHelper r1 r2 mut = do v1 <- readData ms r1
                             v2 <- readData ms r2
                             writeData (mut v1 v2)
      writeData v = writeArray ms pc v

runWorld :: World -> IO World
runWorld w = do
  w' <- step w
  loop w'
  where
    loop n@(World pc _ _ _ _)
        | pc == 0  = return n
        | otherwise = do n' <- step n
                         loop n'

updateWorld :: World -> (Ports -> Ports) -> World
updateWorld (World pc sr ports is ms) f = World pc sr (f ports) is ms

-- Convert an OBF to a World
obfToWorld :: OBF -> Int -> IO World
obfToWorld (OBF is ds) cfg = do
    memory <- newMemory ds
    let instrs = mkInstructions is
    return $ World 0 Off (newPorts $ fromIntegral cfg) instrs memory

-- Run an .obf
readWorld :: FilePath -> Int -> IO World
readWorld filename cfg = do
    obf @ (OBF instructions datas) <- readOBF filename
    --hPutStrLn stderr $ "Read the this many instructions:" ++ (show . length $ instructions)
    --hPutStrLn stderr $ "Read the this many data:" ++ (show . length $ datas)
    --hPutStrLn stderr $ "Read the following instructions:" ++ (show instructions)
    --hPutStrLn stderr $ "Read the following data: " ++ (show datas)
    world <- obfToWorld obf cfg
    return world

