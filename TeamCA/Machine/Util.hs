module TeamCA.Machine.Util
    (
      extractOper
    , extractLower
    , extractOpImm
    ) where

import Data.Bits
import Data.Word

fromPair :: Integral a => (a, a) -> (Int, Int)
fromPair (x, y) = (fromIntegral x, fromIntegral y)

-- |This extracts the upper four bits of a Word32
extractOper :: Word32 -> Int
extractOper w = fromIntegral $ (w .&. 0xf0000000) `shiftR` 28

-- |This extracts the lowert 28 bits of a Word32 as a pair of Ints. For a D-Type
-- instruction, this would be like (r1, r2), for an S-Type instruction this
-- would be like (Op|Imm, r1).
extractLower :: Word32 -> (Int, Int)
extractLower w = fromPair (hi, lo)
    where
      hi = (w .&. 0xf0000000) `shiftR` 14
      lo = w .&. 0x3fff

-- |This extracts a 14 bit quantity in the bit range [27, 14] into the pair
-- (op, imm) as represented by Ints.
extractOpImm :: Int -> (Int, Int)
extractOpImm w
    | w >= 0x4000 = error ("Invalid OpImm value: " ++ (show w))
    | otherwise   = fromPair (op, imm)
    where
      op = (w .&. 0xf00) `shiftR` 8
      imm = w .&. 0xff
