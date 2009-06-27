module Main where

import System.Environment (getArgs)

import TeamCA.Machine.Types
import TeamCA.Machine
import Data.Map

runSimulator :: FilePath -> Int -> (Ports -> Ports) -> IO ()
runSimulator fp cfg mut = do
  world <- readWorld fp cfg
  run world
  return ()
  where
    run w = do w' <- runWorld w
               print w'
               run $ updateWorld w' mut

-- ACCELERATE
strategy :: Ports -> Ports
strategy p0 = pf
    where
      p1 = insert 2 100.0 p0
      p2 = insert 3 100.0 p1
      pf = p2

main = do
  putStrLn "-= ICFP'09 Sim =-"
  args <- getArgs
  case args of
    [] -> error "expecting a file"
    [obfName, config] -> do let cfg = read config :: Int
                            runSimulator obfName cfg strategy
