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

putIEEE754le val = putWord64le . doubleToWord64 $ val

-- A number with n one digits

decodeInstruction :: Word32 -> Either SType DType
decodeInstruction w
    | oper == 0 = Left  $ SType sop imm loReg
    | otherwise = Right $ DType dop hiReg loReg
    where
      mask28 = ones 28
      mask14 = ones 14

      oper = fromIntegral $ shiftR w 28 -- the upper 4 bits
      dop = toEnum $ oper -- the upper 4 bits

      hiReg = fromIntegral hi14
      loReg = fromIntegral lo14

      hi14 = (w .&. mask28) `shiftR` 14 -- upper 14 bits from the lower 28, i.e. bits 14-27
      lo14 = w .&. mask14               -- lower 14 bits, i.e. bits 0-13
      mid10 = hi14 .&. (ones 10)        -- lower 10 bits from lo14

      sop = toEnum $ fromIntegral $ hi14 `shiftR` 10 -- the high 4 bits from hi14
      imm = toEnum $ fromIntegral $ mid10 `shiftR` 7 -- the imm value

      ones n = (1 `shiftL` n) - 1
    
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
