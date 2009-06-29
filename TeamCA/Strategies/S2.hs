module TeamCA.Strategies.S2  where

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
    sArc :: IORef (Maybe Arc),
    sArced :: IORef Bool
}

data Transfer = Transfer Int Vector

newHohmannTransfer = do
    s <- newIORef []
    w <- newIORef Nothing
    b <- newIORef False
    return $ HohmannTransfer s w b


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
                print output
                return Nothing

        saveOutput :: IO ()
        saveOutput = do
            outputs <- getOutputs
            writeIORef (sOutputs strategy) $ take 10 $ output : outputs

        getOutputs :: IO [Output]
        getOutputs = readIORef . sOutputs $ strategy

        boost b = return $ writePort 2 (fst b) $ writePort 3 (snd b) origInput

        origInput = newPorts 0

        nextInputPorts :: IO InputPorts
        nextInputPorts = do
            mArc <- readIORef (sArc strategy)
            arced <- readIORef (sArced strategy)
            case (mArc, arced) of 
                (Just arc, _)  -> followArc arc
                (_, True) -> boost (0, 0)
                otherwise -> seekArc

        seekArc = do
            outputs <- getOutputs
            let pSat1 = oPos . head $ outputs
            let pSat2 = oPosTarget . head $ outputs
            let rSat1 = fst . oPosPolar . head $ outputs
            let rSat2 = fst . oPosTargetPolar . head $ outputs
            let t = hohTime rSat1 rSat2 
            case computeJump t pSat1 pSat2 of 
                Just (v1, v2) -> startArc $ Arc v1 v2 t
                Nothing -> boost (0, 0)

        startArc arc@(Arc v1 v2 t) = do 
            print "start arc"
            print arc
            writeIORef (sArc strategy) (Just arc)
            writeIORef (sArced strategy) True
            boost v1
        
        followArc arc@(Arc v1 v2 t) = do
            writeIORef (sArc strategy) arc'
            if end 
                then print "end arc"
                else return ()
            boost (-fst b, -snd b)
            where 
                arc' = if end then Nothing else Just $ Arc v1 v2 (t - 1.0)
                b = if end then v2 else (0, 0) 
                end = t < 1 


-- Booost Launch off value
adj = -2500.0

oppositeRadian x = fixRad $ x + pi

fixRad r | r >= 0 && r <= (2.0 * pi) = r
         | r < 0 = error "unexpected negative radian"
         | otherwise = fixRad (r -  (2.0 * pi))

data Output = Output {
    oScore :: Double,
    oFuel :: Double,
    oPos :: Point,
    oPosPolar :: Vector,
    oPosTarget :: Point,
    oPosTargetPolar :: Vector
} deriving (Typeable, Data, Ord, Eq, Show)

toOutput oports = Output score fuel pos posPolar posTarget posTargetPolar
    where
          score = look 0x0
          fuel = look 0x1
          pos = (look 0x2, look 0x3)
          posPolar = toPolar pos
          look key = readPort key oports
          relPosTarget = (look 0x4, look 0x5)
          posTargetPolar = toPolar posTarget
          posTarget = (fst pos - fst relPosTarget, snd pos - snd relPosTarget)

data S2 = S2 Int

instance Scenario S2 where
   outputPortsToJSON scenario ports = encodeJSON . toOutput $ ports
   config (S2 i) = i
