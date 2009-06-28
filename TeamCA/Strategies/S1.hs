module TeamCA.Strategies.S1  where

import Data.Map (findWithDefault)
import Data.IORef
import TeamCA.Machine.Types (readPort, outputPorts, inputPorts, Ports, newPorts, writePort)
import TeamCA.Strategies.Types
import TeamCA.Math

data EmptyStrategy = EmptyStrategy

instance Strategy EmptyStrategy where
    store s oports = do
        return False
    next s = return $ newPorts 1001

data RealStrategy = RealStrategy {
    sOutputs :: IORef [Output]
}

newRealStrategy = do 
    s <- newIORef [] 
    return $ RealStrategy s
    
instance Strategy RealStrategy where
    store strategy outputPorts = do
        let output = toOutput outputPorts
        print output
        outputs <- readIORef $ sOutputs strategy
        let outputs' = take 5 $ output : outputs
        writeIORef (sOutputs strategy) outputs'
        print $ "polar: " ++ (show . toPolar . oPos $ output)
        return $ (oScore output) /= 0
   
    next strategy = do 
        outputs <- readIORef $ sOutputs strategy
        let num = length outputs
        let origInput = newPorts 1001
        return $ writePort 2 (1000.0) origInput

data Output = Output { 
    oScore :: Double,
    oFuel :: Double,
    oPos :: (Double, Double),
    oRadius :: Double
} deriving (Ord, Eq, Show)

toOutput oports = Output score fuel pos radius
    where 
          score = look 0x0
          fuel = look 0x1 
          pos = (look 0x2, look 0x3)
          radius = look 0x4
          look key = readPort key oports
