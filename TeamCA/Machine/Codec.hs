module TeamCA.Machine.Codec where

import Data.Binary
import Data.Bits
import Data.Binary.Get
import qualified Data.ByteString
import Data.ByteString.Lazy (fromChunks)

import TeamCA.Machine.Types
import TeamCA.Machine.Util


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
getDoubleIEEEle :: Get Double
getDoubleIEEEle = fmap word64ToDouble getWord64le

getInstruction :: Get (Either SType DType)
getInstruction = fmap decodeInstruction getWord32le

readOBF :: FilePath -> IO OBF
readOBF = decodeFile

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
                                datum <- getDoubleIEEEle
                                instr <- getInstruction
                                obf @(OBF instrs datas) <- get
                                return $ OBF (instr : instrs) (datum : datas)
                        else do
                                instr <- getInstruction
                                datum <- getDoubleIEEEle
                                obf @(OBF instrs datas) <- get
                                return $ OBF (instr : instrs) (datum : datas)
