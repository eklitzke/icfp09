module TeamCA.Strategies.S1  where

import Data.Map (findWithDefault)
import Data.IORef
import TeamCA.Machine.Types (readPort, InputPorts, OutputPorts, outputPorts, inputPorts, Ports, newPorts, writePort)
import TeamCA.Strategies.Types
import TeamCA.Math
import Text.JSON.Generic (encodeJSON)
import Data.Data (Data)
import Data.Typeable (Typeable)

data EmptyStrategy = EmptyStrategy

instance Strategy EmptyStrategy where
    next s o = return $ Just $ newPorts 1001

data HohmannTransfer = HohmannTransfer {
    sOutputs :: IORef [Output],
    sStartAngle :: IORef (Maybe Double)
}

newHohmannTransfer = do 
    s <- newIORef [] 
    a <- newIORef Nothing
    return $ HohmannTransfer s a
    
instance Strategy HohmannTransfer where
    next strategy outputPorts = do 
          saveOutput  
          if oScore output /= 0 
            then done 
            else fmap Just nextInputPorts
      where 
        output :: Output
        output = toOutput outputPorts

        done :: IO (Maybe InputPorts)
        done = do
                print "done"
                --print output
                return Nothing

        saveOutput :: IO ()
        saveOutput = do 
            outputs <- getOutputs
            writeIORef (sOutputs strategy) $ output : outputs

        getOutputs :: IO [Output]
        getOutputs = readIORef . sOutputs $ strategy
        
        setStartAngle :: Double -> IO ()
        setStartAngle angle = writeIORef (sStartAngle strategy) (Just angle)

        (currRadius, currAngle) = toPolar . oPos $ output

        initialBoost :: IO InputPorts
        initialBoost = do 
            print "initial boost"
            setStartAngle currAngle
            --print "Initial boost position:"
            --printPosition
            (x, y) <- getBoostXY 
            return $ writePort 2 x $ writePort 3 y origInput

        getBoostXY :: IO (Double, Double)
        getBoostXY = return (0.0, -2500.0)

        velocity :: IO (Double, Double)
        velocity = do 
            outputs <- getOutputs
            let i = head outputs
            let j = head . tail $ outputs
            let k = onVector2 (-) (oPos i) (oPos j)
            if length outputs < 2
                then return (0.0, 0.0)
                else return k

        acceleration :: IO (Double, Double)
        acceleration = do 
            outputs <- getOutputs
            let i = head outputs
            let j = head . tail $ outputs
            let k = head . tail . tail $ outputs
            let m = onVector2 (-) (oPos i) (oPos j)
            let n = onVector2 (-) (oPos j) (oPos k)
            let o = onVector2 (-) m n 
            if length outputs < 3
                then return (0, 0)
                else return o

        printPosition :: IO ()
        printPosition = do 
            print output
            print $ toPolar $ oPos $ output
    
        origInput = newPorts 1001
        targetRadius = oRadius output

        nextInputPorts :: IO InputPorts
        nextInputPorts = do 
            outputs <- getOutputs
            if length outputs == 2
                then initialBoost
                else do 
                    maybeAngle <- readIORef $ sStartAngle strategy
                    case maybeAngle of
                        Nothing -> return origInput
                        Just angle -> do
                            let oppAngle = oppositeRadian angle 
                            let angleDiff = abs $ oppAngle - currAngle 
                            let radiusDiff = abs $ currRadius - targetRadius
                            if angleDiff <= 0.01 && radiusDiff < 1000.0
                                then reverseBoost
                                else return origInput

        clearStartAngle = do 
            writeIORef (sStartAngle strategy) Nothing

        reverseBoost = do 
            print "end"
            clearStartAngle
            return $ writePort 3 (-adj) origInput

-- Booost Launch off value 
adj = -2500.0

oppositeRadian x = fixRad $ x + pi

fixRad r | r >= 0 && r <= (2.0 * pi) = r
         | r < 0 = error "unexpected negative radian"
         | otherwise = fixRad (r -  (2.0 * pi)) 

data Output = Output { 
    oScore :: Double,
    oFuel :: Double,
    oPos :: (Double, Double),
    oPosPolar :: (Double, Double),
    oRadius :: Double
} deriving (Typeable, Data, Ord, Eq, Show)

toOutput oports = Output score fuel pos posPolar radius
    where 
          score = look 0x0
          fuel = look 0x1 
          pos = (look 0x2, look 0x3)
          posPolar = toPolar pos
          radius = look 0x4
          look key = readPort key oports

data S1 = S1 Int

instance Scenario S1 where
   outputPortsToJSON scenario ports = encodeJSON . toOutput $ ports
   config (S1 i) = i
