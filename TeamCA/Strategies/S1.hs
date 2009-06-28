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
    sOutputs :: IORef [Output],
    sStartAngle :: IORef (Maybe Double)
}

newRealStrategy = do 
    s <- newIORef [] 
    a <- newIORef Nothing
    return $ RealStrategy s a
    
instance Strategy RealStrategy where
    store strategy outputPorts = do
        let output = toOutput outputPorts
        --print output
        outputs <- readIORef $ sOutputs strategy
        let outputs' = take 5 $ output : outputs
        writeIORef (sOutputs strategy) outputs'
        --print $ "polar: " ++ (show . toPolar . oPos $ output)
        return $ (oScore output) /= 0
   
    next strategy = do 
        outputs <- readIORef $ sOutputs strategy
        let num = length outputs
        let origInput = newPorts 1001
        let output = head outputs
        let (currRadius, currAngle) = toPolar $ oPos $ head outputs
        let targetRadius = oRadius $ head outputs
        print $ toPolar $ oPos $ head outputs

        if num == 1
            then do 
                    writeIORef (sStartAngle strategy) (Just currAngle)
                    print "start"
                    print output
                    print $ toPolar $ oPos $ head outputs
                    return $ writePort 3 adj origInput
            else do 
                maybeAngle <- readIORef $ sStartAngle strategy
                case maybeAngle of
                    Nothing -> return origInput
                    Just angle -> do
                        let oppAngle = oppositeRadian angle 
                        let angleDiff = abs $ oppAngle - currAngle 
                        let radiusDiff = abs $ currRadius - targetRadius
                        if angleDiff <= 0.01 && radiusDiff < 1000.0
                            then do
                                print "end"
                                print output
                                print $ toPolar $ oPos $ head outputs
                                writeIORef (sStartAngle strategy) Nothing
                                return $ writePort 3 (-adj) origInput
                            else return origInput


-- Launch off value 
-- Boost 
adj = -2600.0

oppositeRadian x = fixRad $ x + pi

fixRad r | r >= 0 && r <= (2.0 * pi) = r
         | r < 0 = error "unexpected negative radian"
         | otherwise = fixRad (r -  (2.0 * pi)) 

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
