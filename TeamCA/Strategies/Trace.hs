module TeamCA.Strategies.Trace (TraceStrategy(..)) where

import TeamCA.Strategies.Types (Strategy, next, Scenario, outputPortsToJSON)
import System.IO

data TraceStrategy = forall a b. (Strategy a, Scenario b)  => TraceStrategy a b Handle

instance Strategy TraceStrategy where 
    next (TraceStrategy sub scenario handle) ports = do
        hPutStrLn handle $ outputPortsToJSON scenario ports
        next sub ports
