module TeamCA.Strategies.Trace (OutputTrace(..)) where

import TeamCA.Strategies.Types (Strategy, next, Scenario, outputPortsToJSON)
import System.IO

data OutputTrace = forall a b. (Strategy a, Scenario b)  => OutputTrace a b Handle

instance Strategy OutputTrace where 
    next (OutputTrace sub scenario handle) ports = do
        hPutStrLn handle $ outputPortsToJSON scenario ports
        next sub ports
