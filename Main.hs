module Main where

import System.Environment (getArgs)

import TeamCA.Machine.Types
import TeamCA.Machine
import Data.Map
import qualified TeamCA.Strategies.S1 as S1

runSimulator :: FilePath -> Int -> (Ports -> Ports) -> IO ()
runSimulator fp cfg mut = do
  world <- readWorld fp cfg
  run world
  return ()
  where
    run w = do w' <- runWorld w
               print $ S1.output w'
               run $ updateWorld w' mut

-- ACCELERATE
strategy :: Ports -> Ports
strategy p0 = pf
    where
      p1 = insert 2 0.1 p0
      p2 = insert 3 0.1 p1
      pf = p2


main = do
  putStrLn "-= ICFP'09 Sim =-"
  args <- getArgs
  case args of
    [] -> error "expecting a file"
    [obfName, config] -> do let cfg = read config :: Int
                            runSimulator obfName cfg strategy
