module Main where

import System.Environment (getArgs)

import TeamCA.Machine.Types
import TeamCA.Strategies.Types (Strategy)
import TeamCA.Machine
import Data.Map
import qualified TeamCA.Strategies.S1 as S1
import TeamCA.Strategies.Types (store, next, isDone)

runSimulator :: Strategy s => FilePath -> Int -> s -> IO ()
runSimulator fp cfg strat = do
  world <- readWorld fp cfg
  run world
  return ()
  where
    run w = do 
        w' <- runWorld w
        store strat (outputPorts w')
        if isDone strat 
            then return ()
            else do
                inputPorts <- next strat 
                run $ updateWorld w' inputPorts

main = do
  putStrLn "-= ICFP'09 Sim =-"
  args <- getArgs
  case args of
    [] -> error "expecting a file"
    [obfName, config] -> do let cfg = read config :: Int
                            runSimulator obfName cfg S1.defaultStrategy
