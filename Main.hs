module Main where

import System.Environment (getArgs)

import TeamCA.Machine.Types
import TeamCA.Machine
import Data.Map

configurationPort :: Int
configurationPort = 16000

runSimulator :: FilePath -> Int -> (Ports -> Ports) -> IO ()
runSimulator fp cfg mut = do
  world <- readWorld fp
  run (updateWorld world configure)
  return ()
  where
    run w = do w' <- runWorld w
               print w'
               run $ updateWorld w' mut

    configure :: Ports -> Ports
    configure = insert configurationPort (fromIntegral cfg)

strategy :: Ports -> Ports
strategy = id

main = do
    print "starting simulator"
    args <- getArgs
    case args of
        [] -> error "expecting a file"
        [obfName, config] -> do let cfg = read config :: Int
                                runSimulator obfName cfg strategy
