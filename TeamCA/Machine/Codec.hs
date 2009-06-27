module TeamCA.Machine.Codec where

import Data.Binary
import Data.Bits
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import qualified Data.ByteString
import Data.ByteString.Lazy (fromChunks)
import qualified Data.Map

import TeamCA.Machine.Types
import TeamCA.Machine.Util

-- Reduce redundant and empty frames
-- If a frame has an empty port value, remove it
reduceFrames :: [Frame] -> [Frame]
reduceFrames [] = []
reduceFrames (x@(Frame _ p) : [])
                | (Data.Map.size p) > 0 = [x]
                | otherwise  = []
reduceFrames (x@(Frame _ p1) : (x'@(Frame _ p2) : xs)) 
    | (Data.Map.size p1) == 0 = reduceFrames $ x' : xs
    | p1 == p2 = reduceFrames $ x : xs
    | otherwise = x : (reduceFrames $ x' : xs)

instance Binary Solution where
    put (Solution team scenario frames) = do
        putWord32le . fromIntegral $ 0xCAFEBABE
        putWord32le . fromIntegral $ team
        putWord32le . fromIntegral $ scenario
        mapM_ put frames
    get = undefined

instance Binary Frame where
    get = undefined
    put (Frame ts port) = do 
        putWord32le . fromIntegral $ ts
        putWord32le . fromIntegral $ Data.Map.size port
        forM_ (Data.Map.toList port) $ \(addr, val) -> do 
            putWord32le . fromIntegral $ addr
            putIEEE754le val

writeSolution :: FilePath -> Solution -> IO ()
writeSolution = encodeFile

putIEEE754le val = putWord64le . doubleToWord64 $ val

decodeInstruction :: Word32 -> Either SType DType
decodeInstruction w
    | oper == 0 = Left  $ SType (toEnum oper') (toEnum imm) lAddr'
    | otherwise = Right $ DType (toEnum oper) hAddr' lAddr'
    where
      oper = extractOper w
      (hAddr, lAddr) = extractLower w
      (oper', imm) = extractOpImm hAddr
      hAddr' = fromIntegral hAddr
      lAddr' = fromIntegral lAddr

word64ToDouble :: Word64 -> Double
word64ToDouble = decodeIEEE 11 52

doubleToWord64 = encodeIEEE 11 52

encodeIEEE :: (RealFloat a, Bits b, Integral b) => Int -> Int -> a -> b
encodeIEEE exponentBits significandBits f =
      (signBit `shiftL` (exponentBits + significandBits)) .|.
     (exponentField `shiftL` significandBits) .|.
      significandField
   where (significand, exponent) = decodeFloat f

         signBit | significand < 0 = 1
                 | otherwise = 0
         exponentField | significand == 0 && exponent == 0 = 0
                       | otherwise = fromIntegral exponent + exponentBias + fromIntegral significandBits
         significandField = fromIntegral (abs significand) .&. significandMask

         exponentBias = bit (exponentBits - 1) - 1
         significandMask = bit significandBits - 1


-- Decode an IEEE Double from num exponents, num significant bits and a word64
decodeIEEE :: (Bits a, Integral a, RealFloat b) => Int -> Int -> a -> b
decodeIEEE exponentBits significandBits n = encodeFloat significand exponent
   where significand = adjustSign (fromIntegral (adjustSignificand significandField))
         exponent = exponentField - exponentBias - significandBits

         adjustSign = if n `testBit` (exponentBits + significandBits) then negate else id
         adjustSignificand = if exponentField > 0 then (`setBit` significandBits) else id

         exponentBias = bit (exponentBits - 1) - 1
         exponentField = fromIntegral ((n `shiftR` significandBits) .&. exponentMask)
         exponentMask = bit exponentBits - 1

         significandField = n .&. significandMask
         significandMask = bit significandBits - 1

-- Get a IEEE Double
getIEEE754le :: Get Double
getIEEE754le = fmap word64ToDouble getWord64le

getInstruction :: Get (Either SType DType)
getInstruction = fmap decodeInstruction getWord32le

readOBF :: FilePath -> IO OBF
readOBF fp = decodeFile fp

data OBF = OBF [Instruction] [Double]

instance Binary OBF where
    put a = undefined
    get = do
        e <- isEmpty
        if e
            then return $ OBF [] []
            else do
                    b <- bytesRead
                    if ((b `div` 12) `mod` 2) == 0
                        then do
                                datum <- getIEEE754le
                                instr <- getInstruction
                                obf @(OBF instrs datas) <- get
                                return $ OBF (instr : instrs) (datum : datas)
                        else do
                                instr <- getInstruction
                                datum <- getIEEE754le
                                obf @(OBF instrs datas) <- get
                                return $ OBF (instr : instrs) (datum : datas)
