
import System.IO
import System.Environment (getArgs)
import Data.Binary
import Data.Bits
import Control.Monad (liftM) 
import Data.Binary.Get
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromChunks)

main = do
    print "starting simulator"
    args <- getArgs
    case args of
        [] -> error "expecting a file"
        x -> mapM run x
    return ()

run filename = do
    bs <- BS.readFile filename
    let (instructions, datas) = runGet getInstructionsAndData $ fromChunks [bs]
    hPutStrLn stderr $ "read instrs:" ++ (show $ length instructions) ++ ", datas: " ++ (show $ length datas)
    hPutStrLn stderr $ "ds" ++ (show datas)

word64ToDouble :: Word64 -> Double
word64ToDouble = decodeIEEE 11 52

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

getDoubleIEEEle = liftM word64ToDouble getWord64le

getInstructionsAndData :: Get ([Word32], [Double])
getInstructionsAndData = do
    e <- isEmpty
    if e
        then return ([], [])
        else do
                b <- bytesRead
                if ((b `div` 96) `mod` 2) == 0
                    then do
                            datum <- getDoubleIEEEle
                            instr <- getWord32le
                            (instrs, datas) <- getInstructionsAndData
                            return $ (instr : instrs, datum : datas)
                    else do
                            instr <- getWord32le
                            datum <- getDoubleIEEEle
                            (instrs, datas) <- getInstructionsAndData
                            return $ (instr : instrs, datum : datas)

