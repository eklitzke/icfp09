module Main where

import System.Environment (getArgs)


import Data.Array (elems)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map hiding (elems)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.IO (openFile, hClose, IOMode(..))
import TeamCA.Machine
import TeamCA.Machine.Types
import TeamCA.Strategies.Trace (TraceStrategy(..))
import TeamCA.Strategies.Types (Strategy)
import TeamCA.Strategies.Types (next)
import qualified TeamCA.Strategies.S1 as S1

runSimulator :: Strategy s => FilePath -> Int -> s -> IO ()
runSimulator fp cfg strat = do
  world@(World _ _ _ _ is ms) <- readWorld fp cfg
  runSim world
  where
    runSim w = do 
      w' <- runWorld w
      maybeInput <- next strat (outputPorts w')
      case maybeInput of
        Just inputPorts -> runSim $ updateWorld w' inputPorts
        Nothing -> return ()

data Options = Options {
    optTrace :: Maybe String
}

options :: [OptDescr (Options -> Options)]
options = [
            (Option ['t'] ["trace"] (OptArg traceArg "FILE") "trace FILE")
    ]   
traceArg maybeF opts = opts {optTrace=maybeF}
    

defaultOptions = Options { optTrace = Nothing}

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = 
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: vm [OPTION...] files..."


-- helper to pass around typeclassed values
data S = forall a. Strategy a => S a

main = do
  putStrLn "-= ICFP'09 Sim =-"
  (opts, args) <- getArgs >>= parseOpts
  s <- S1.newRealStrategy 
  refMaybeTraceH <- newIORef Nothing
  let scenario = S1.S1 1001
  s' <- case optTrace opts of
          Just traceFile -> do 
                   traceH <- openFile traceFile WriteMode
                   writeIORef refMaybeTraceH $ Just traceH 
                   return $ S $ TraceStrategy s scenario traceH  
          Nothing -> return $ S s
  
  if length args /= 2
      then error "usage: vm <obf file> <configuration>"
      else let [obfName, config] = args in
           let cfg = read config :: Int in
           case s' of S str -> runSimulator obfName cfg str

  maybeTraceH <- readIORef refMaybeTraceH
  case maybeTraceH of
      Just traceH -> hClose traceH
      Nothing -> return ()
