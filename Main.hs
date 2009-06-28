module Main where

import System.Environment (getArgs)

import TeamCA.Machine.Types
import TeamCA.Strategies.Types (Strategy)
import TeamCA.Machine
import Data.Map
import qualified TeamCA.Strategies.S1 as S1
import TeamCA.Strategies.Types (store, next)

runSimulator :: Strategy s => FilePath -> Int -> s -> IO ()
runSimulator fp cfg strat = do
  print "inital cfg"
  print cfg
  world <- readWorld fp cfg
  run world
  where
    run w = do 
        w' <- runWorld w
        isDone <- store strat (outputPorts w')
        if isDone 
            then return ()
            else do
                inputPorts <- next strat 
                run $ updateWorld w' inputPorts

main = do
  putStrLn "-= ICFP'09 Sim =-"
  args <- getArgs
  s <- S1.newRealStrategy 
  if length args /= 2
      then error "usage: vm <obf file> <configuration>"
      else let [obfName, config] = args in
           let cfg = read config :: Int in
           runSimulator obfName cfg  s
