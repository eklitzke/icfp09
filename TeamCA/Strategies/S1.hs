module TeamCA.Strategies.S1 (strategy, output) where

import Data.Map (findWithDefault)

import TeamCA.Machine.Types (readPort, outputPorts, inputPorts, Ports)

strategy s1 = s1

-- A strategy is something you implement to run the simulation. It reads the
-- output ports, and then writes to input ports to return a set of new port
-- values. The world is then re-run with thse new ports.
type Strategy = Ports -> Ports

data Output = Output { 
    oScore :: Double,
    oFuel :: Double,
    oPos :: (Double, Double),
    oRadius :: Double
} deriving (Ord, Eq, Show)

output world = Output score fuel pos radius
    where 
          score = look 0x0
          fuel = look 0x1 
          pos = (look 0x2, look 0x3)
          radius = look 0x4
          look key = readPort key (outputPorts world)
