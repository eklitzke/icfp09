module TeamCA.Session where

import Data.IORef
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad
import qualified Data.Map

import TeamCA.Machine.Codec
import TeamCA.Machine.Types (InputPorts)

data Session = Session {
    sessTeam :: Int,
    sessScenarioID :: Int,
    sessFrames :: [Frame],
    sessTick :: Int
}

type Timestep = Int

instance Binary Session where
    put sess = do
        putWord32le . fromIntegral $ 0xCAFEBABE
        putWord32le . fromIntegral $ sessTeam sess
        putWord32le . fromIntegral $ sessScenarioID sess
        mapM_ put (reverse . sessFrames $ sess)
        putWord32le . fromIntegral $ (sessTick sess)
        putWord32le . fromIntegral $ 0

    get = undefined

instance Binary Frame where
    get = undefined
    put (Frame ts x y) = do 
        putWord32le . fromIntegral $ ts
        putWord32le $ fromIntegral 2
        putWord32le $ fromIntegral 2
        putIEEE754le x
        putWord32le $ fromIntegral 3
        putIEEE754le y

data Frame = Frame Timestep Double Double
    deriving (Ord, Eq, Show)

newSession team scenario = Session team scenario [] (-1)

addInput :: InputPorts -> Session -> Session
addInput input session = session{sessFrames=f', sessTick=t'}
    where 
        f = sessFrames session
        t' = 1 + sessTick session
        fr = Frame t' x y
        f' = case f of 
                [] -> [fr]
                ((Frame t x' y') : xs) -> 
                    if (x /= x' || y /= y') then (fr : f) else f
        x = Data.Map.findWithDefault 0.0 2 input
        y = Data.Map.findWithDefault 0.0 3 input

