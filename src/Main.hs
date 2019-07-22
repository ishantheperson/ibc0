{-# LANGUAGE MultiWayIf #-}
import ParseIt (getProgram)
-- import EvalIt (runProgram)
-- import Repl (runRepl)
import CodegenBC0 (compileFile)

import Data.Either (lefts)

import System.Environment (getArgs)
import System.Console.GetOpt 
import System.Exit (exitSuccess, exitFailure)

data Flag = GenBytecode | Help | RunRepl deriving (Show, Eq) 

main :: IO () 
main = do 
  args <- getArgs 

  case getOpt Permute options args of 
    (opts, files, []) -> 
      if | Help `elem` opts -> printHelp "Simple BC0 compiler" 
         | RunRepl `elem` opts -> do putStrLn "The REPL is currently not available" -- runRepl 
                                     exitFailure

         | length files == 0 -> do printHelp "Error: missing filenames"
                                   exitFailure

         | GenBytecode `elem` opts -> mapM_ compileFile files 
{-
         | otherwise -> do programResults <- mapM runFile files
                           let runtimeErrors = lefts programResults
                           
                           if null runtimeErrors then 
                             exitSuccess
                           else do 
                             mapM_ print runtimeErrors 
                             exitFailure
-}
    (_, _, errors) -> printHelp $ concat errors

  where printHelp e = putStrLn e >> putStr (usageInfo "usage: ./simple (flags) (filenames). By Ishan Bhargava" options)
        -- runFile fileName = readFile fileName >>= runProgram . getProgram 
        runFile fileName = putStrLn $ "couldn't run '" ++ fileName ++ "'. The interpreter is currently not available."

        options = [ Option ['h', '?'] ["help"] (NoArg Help) "print this help message",
                    Option ['b'] ["bytecode"] (NoArg GenBytecode) "generate BC0",
                    Option ['i'] ["interactive", "repl"] (NoArg RunRepl) "launch REPL" ]

