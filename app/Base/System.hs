module Base.System
  ( ExitCode (ExitFailure),
    doesFileExist,
    exitSuccess,
    exitWith,
    getArgs,
    hPutStrLn,
    newStdGen,
    randomR,
    stderr,
    threadDelay,
  )
where

import Control.Concurrent (threadDelay)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen, randomR)
