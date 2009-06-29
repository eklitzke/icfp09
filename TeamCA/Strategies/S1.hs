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
    sWait :: IORef (Maybe Int),
    sBoost2 :: IORef (Maybe Vector)
}

newHohmannTransfer = do 
    s <- newIORef [] 
    w <- newIORef Nothing
    b <- newIORef Nothing
    return $ HohmannTransfer s w b

    
instance Strategy HohmannTransfer where
    next strategy outputPorts = do 
          decrWait
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
                print output
                return Nothing

        saveOutput :: IO ()
        saveOutput = do 
            outputs <- getOutputs
            writeIORef (sOutputs strategy) $ output : outputs

        getOutputs :: IO [Output]
        getOutputs = readIORef . sOutputs $ strategy
        
        (currRadius, currAngle) = toPolar . oPos $ output

        initialBoost :: IO InputPorts
        initialBoost = do 
            print "initial boost"
            (boost1, boost2, wait) <- getBoosts
            print boost1
            print boost2
            print wait
            setWait $ fromIntegral $ round $ wait
            setBoost2 boost2
            boost boost1

        boost b = return $ writePort 2 (fst b) $ writePort 3 (snd b) origInput

        setWait :: Int -> IO ()
        setWait i = writeIORef (sWait strategy) (Just i)

        setBoost2 vector = writeIORef (sBoost2 strategy) (Just vector)
            
        readyForBoost2 :: IO Bool
        readyForBoost2 = do
            maybeI <- readIORef (sWait strategy) 
            case maybeI of 
                Just 0 -> return True
                otherwise -> return False
        
        getBoost2 :: IO (Maybe Vector)
        getBoost2 = readIORef (sBoost2 strategy)

        decrWait = do 
            maybeI <- readIORef (sWait strategy)
            writeIORef (sWait strategy) $
                case maybeI of 
                   Nothing -> Nothing
                   Just 0  -> Nothing
                   Just i  -> Just (i - 1)  
         
        getBoosts :: IO (Vector, Vector, Double)
        getBoosts = do 
            o <- fmap head getOutputs
            currV <- getVelocity
            let r1 = fst currV
            let r2 = oRadius o
            let boost1 = delta1 r1 r2 currV
            let boost2 = delta2 r1 r2 currV
            let t =  hohTime r1 r2
            return $ (boost1, boost2, t) 

        getVelocity :: IO (Double, Double)
        getVelocity = do 
            outputs <- getOutputs
            let i = head outputs
            let j = head . tail $ outputs
            let k = onVector2 (-) (oPos i) (oPos j)
            if length outputs < 2
                then return (0.0, 0.0)
                else return k

        getAcceleration :: IO (Double, Double)
        getAcceleration = do 
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

        mu = 389600.0
        mu8 = 8.0 * mu

        printPosition :: IO ()
        printPosition = do 
            print output
            print $ toPolar $ oPos $ output
    
        origInput = newPorts 1001
        targetRadius = oRadius output

        nextInputPorts :: IO InputPorts
        nextInputPorts = do 
            outputs <- getOutputs
            ready <- readyForBoost2
            mBoost2 <- getBoost2
            case (length outputs, ready, mBoost2) of
                (2, _, _)    -> initialBoost
                (_, True, Just b) -> boost b
                otherwise -> boost (0, 0)

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
