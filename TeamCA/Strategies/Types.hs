module TeamCA.Strategies.Types
import TeamCA.Machine.Types (World, Ports)

class Strategy s where 
    next :: s -> World -> IO Ports
