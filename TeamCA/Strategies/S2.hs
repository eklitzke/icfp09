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
    sArc :: IORef (Maybe Arc),
    sArced :: IORef Bool
}

data Transfer = Transfer Int Vector

newHohmannTransfer = do
    w <- newIORef Nothing
    b <- newIORef False
    return $ HohmannTransfer w b

instance Strategy HohmannTransfer where
    next strategy outputPorts = do
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
            let pSat1 = oPos $ output
            let pSat2 = oPosTarget $ output
            let rSat1 = fst . oPosPolar $ output
            let rSat2 = fst . oPosTargetPolar $ output
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
                then do 
                    print "end arc"
                    print satDist
                else return ()
            boost (-fst b, -snd b)
            where 
                arc' = if end then Nothing else Just $ Arc v1 v2 (t - 1.0)
                b = if end then v2 else (0, 0) 
                end = t <= -1
        
        satDist = dist (oPos output) (oPosTarget output)

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
