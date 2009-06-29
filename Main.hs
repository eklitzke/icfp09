module Main where

import System.Environment (getArgs)


import Data.Array (elems)
import Data.Binary (encodeFile)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef, IORef)
import Data.Map hiding (elems)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.IO (openFile, hClose, IOMode(..))
import TeamCA.Machine
import TeamCA.Machine.Types
import TeamCA.Strategies.Trace (OutputTrace(..))
import TeamCA.Strategies.Types (Strategy, Scenario, next)
import qualified TeamCA.Session as Session
import qualified TeamCA.Strategies.S1 as S1
import qualified TeamCA.Strategies.S2 as S2

runSimulator ::  Strategy s => IORef Session.Session -> FilePath -> Int -> s -> IO ()
runSimulator sessRef fp cfg strat = do
      world@(World _ _ _ _ is ms) <- readWorld fp cfg
      runSim world
      where
        runSim w = do 
          w' <- runWorld w
          maybeInput <- next strat (outputPorts w')
          case maybeInput of
            Just inputPorts -> do
                modifyIORef sessRef (Session.addInput inputPorts)
                runSim $ updateWorld w' inputPorts
            Nothing -> return ()

data Options = Options {
    optTrace :: String,
    optSession :: String,
    optInput :: String,
    optScenario :: Int,
    optStrategy :: String
}

options :: [OptDescr (Options -> Options)]
options = [
            (Option ['t'] ["trace"] (ReqArg traceArg "FILE") "trace FILE"),
            (Option ['s'] ["session"] (ReqArg sessArg "SESSION") "session SESSIONFILE"),
            (Option ['i'] ["input"] (ReqArg (\x o -> o{optInput=x}) "INPUT") "input INPUTFILE"),
            (Option ['c'] ["scenario"] (ReqArg (\x o -> o{optScenario=read x}) "SCENARIO") "scenario SCENARIO"),
            (Option ['c'] ["strategy"] (ReqArg (\x o -> o{optStrategy=x}) "STRATEGY") "strategy STRATEGY")
    ]   

traceArg f opts = opts {optTrace=f}
sessArg f opts = opts {optSession=f}

defaultOptions = Options { 
    optTrace = "",
    optSession = "",
    optInput = "",
    optScenario = -1,
    optStrategy = ""
    }

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = 
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: vm [OPTION...] files..."

-- helper to pass around typeclassed values
data S = forall a. Strategy a => S a

teamID = 29

main = do
  putStrLn "-= ICFP'09 Sim =-"
  (opts, _) <- getArgs >>= parseOpts
  main' opts

main' opts = do 
  refMaybeTraceH <- newIORef Nothing
  strategy <- case (optStrategy opts) of
    "hohmann" -> fmap S S1.newHohmannTransfer 
    otherwise -> error $ "unknown strategy: " ++ otherwise
  traceH <- openFile (optTrace opts) WriteMode
  let strategy' = 
        case (scenario, strategy) of 
            (Sc y, S x) -> S $ OutputTrace x y traceH
  let obfName = optInput opts
  let cfg = optScenario opts
  sessRef <- newIORef $ Session.newSession teamID cfg
  case strategy' of S str -> runSimulator sessRef obfName cfg str
  sess <- readIORef sessRef
  encodeFile (optSession opts) sess
  hClose traceH
    where 
        scenario :: Sc
        scenario = case optScenario opts of
                    1001 -> Sc $ S1.S1 1001
                    1002 -> Sc $ S1.S1 1002
                    1003 -> Sc $ S1.S1 1003
                    1004 -> Sc $ S1.S1 1004
                    2001 -> Sc $ S2.S2 2001
                    2002 -> Sc $ S2.S2 2002
                    2003 -> Sc $ S2.S2 2003
                    2004 -> Sc $ S2.S2 2004
                    otherwise -> error "unexpected scenario"

data Sc = forall a. Scenario a => Sc a
