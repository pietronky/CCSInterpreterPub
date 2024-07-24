module Main where

import Base.Char (toLower)
import Base.System (
  ExitCode (ExitFailure), 
  doesFileExist, 
  exitWith,
  exitSuccess, 
  getArgs,
  hPutStrLn,
  stderr,
  threadDelay)
import Context (Definitions, checkMain, forceGetMain, prettyDefs)
import Execution (Step, nextStep, prettyStep)
import Parsing.Wrapper (parseCCSEnvironment)
import Process (Proc (Const))
import Translation (translDefs)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then do
    hPutStrLn stderr "Error: usage is <PROGRAM> <FILE PATH>"
    exitWith (ExitFailure 1)
  else do
    isFile <- doesFileExist (head args)
    if not isFile then do
      hPutStrLn stderr ("Error: file \"" ++ head args ++ "\" does not exist")
      exitWith (ExitFailure 2)
    else do
      sourceCode <- readFile (head args)
      case parseCCSEnvironment sourceCode of
        Left msg -> do
          hPutStrLn stderr ("Error:\n" ++ msg)
          exitWith (ExitFailure 3)
        Right ctx -> do
          let env = (\(_, _, thd) -> thd) ctx
          case checkMain env of
            Left msg -> do
              hPutStrLn stderr ("Error:\n" ++ msg)
              exitWith (ExitFailure 4)
            Right _ -> do
              putStr ("Source code:\n" ++ prettyDefs env ++ "\n\n")
              let translations = translDefs ctx
              putStr ("Pure source code:\n" ++ prettyDefs translations ++ "\n\n")
              optionallyExecute translations

execute :: Definitions -> Proc -> IO ()
execute env p = do
  step <- nextStep env p
  case step of
    Left msg -> do
      hPutStrLn stderr ("Error: " ++ msg)
      exitWith (ExitFailure 5)
    Right (a, p') -> do
      slowlyShowStep (a, p')
      execute env p'

optionallyExecute :: Definitions -> IO ()
optionallyExecute env = do
  putStrLn "Want to execute pure source code ? (y/N) "
  yN <- getLine
  if null yN || map toLower yN == "n" then do
    exitSuccess
  else if map toLower yN == "y" then do
    let def = forceGetMain env
    slowlyShowStep (Nothing, Const "Main" Nothing)
    slowlyShowStep (Nothing, def)
    execute env def
  else
    optionallyExecute env

slowlyShowStep :: Step -> IO ()
slowlyShowStep (a, p) = do 
  putStrLn (prettyStep (a, p))
  threadDelay 1000000
