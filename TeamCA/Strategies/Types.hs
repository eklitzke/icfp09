module TeamCA.Stragies.Types

-- A strategy is something you implement to run the simulation. It reads the
-- output ports, and then writes to input ports to return a set of new port
-- values. The world is then re-run with thse new ports.
type Strategy = Ports -> Ports
