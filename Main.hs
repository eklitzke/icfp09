
import System.IO
import System.Info
import System.Environment
import Data.Binary
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

getInstructionsAndData :: Get ([Word32], [Word64])
getInstructionsAndData = do
    e <- isEmpty
    if e
        then return ([], [])
        else do
                b <- bytesRead
                if ((b `div` 96) `mod` 2) == 0
                    then do
                            datum <- getWord64le
                            instr <- getWord32le
                            (instrs, datas) <- getInstructionsAndData
                            return $ (instr : instrs, datum : datas)
                    else do
                            instr <- getWord32le
                            datum <- getWord64le
                            (instrs, datas) <- getInstructionsAndData
                            return $ (instr : instrs, datum : datas)

