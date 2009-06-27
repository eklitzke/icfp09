module Main where

import System.Environment (getArgs)
import TeamCA.Machine (run)

main = do
    print "starting simulator"
    args <- getArgs
    case args of
        [] -> error "expecting a file"
        x -> mapM run x
    return ()
