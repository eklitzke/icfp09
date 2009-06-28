module TeamCA.Strategies.Types 
    (
        Strategy
        , next
        , isDone
        , store
    ) where

import TeamCA.Machine.Types (World, OutputPorts, InputPorts)

-- A strategy is something you implement to run the simulation. It reads the
-- output ports, and then writes to input ports to return a set of new port
-- values. The world is then re-run with thse new ports.

class Strategy s where 
    next :: s -> IO InputPorts
    store :: s -> OutputPorts -> IO ()
    isDone :: s -> Bool

