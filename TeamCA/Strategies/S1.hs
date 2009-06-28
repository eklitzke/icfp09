module TeamCA.Strategies.S1  where

import Data.Map (findWithDefault)

import TeamCA.Machine.Types (readPort, outputPorts, inputPorts, Ports, newPorts)
import TeamCA.Strategies.Types

data EmptyStrategy = EmptyStrategy

instance Strategy EmptyStrategy where
    store s oports = do
        print "store"
        print $ oports
        print $ toOutput oports

    isDone s = False
    next s = return $ newPorts 1001

defaultStrategy = EmptyStrategy


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
