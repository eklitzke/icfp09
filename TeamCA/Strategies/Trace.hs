module TeamCA.Strategies.Trace (TraceStrategy(..)) where

import TeamCA.Strategies.Types (Strategy, next)

data TraceStrategy = forall a. Strategy a => TraceStrategy a

instance Strategy TraceStrategy where 
    next (TraceStrategy sub) ports = do
        print ports
        next sub ports
