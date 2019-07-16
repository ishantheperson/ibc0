{-# LANGUAGE MultiWayIf #-}
import ParseIt (getProgram)
import EvalIt (runProgram)
import Repl (runRepl)
import CodegenBC0 (compileFile)

import Data.Either (lefts)

import System.Environment (getArgs)
import System.Console.GetOpt 

data Flag = GenBytecode | Help | RunRepl deriving (Show, Eq) 

main :: IO () 
main = do 
  args <- getArgs 

  case getOpt Permute options args of 
    (opts, files, []) -> 
      if | Help `elem` opts -> putStrLn "Simple BC0 compiler" >> printHelp 
         | RunRepl `elem` opts -> runRepl 
         | length files == 0 -> printError "Error: missing filenames"

         | GenBytecode `elem` opts -> mapM_ compileFile files 
         | otherwise -> do programResults <- mapM runFile files
                           let runtimeErrors = lefts programResults
                           mapM_ print runtimeErrors 

    (_, _, errors) -> printError $ concat errors

  where printHelp = putStr $ usageInfo "usage: ./simple (flags) (filenames). By Ishan Bhargava" options 
        printError e = putStrLn e >> printHelp 

        runFile fileName = readFile fileName >>= runProgram . getProgram 

        options = [ Option ['h', '?'] ["help"] (NoArg Help) "print this help message",
                    Option ['b'] ["bytecode"] (NoArg GenBytecode) "generate BC0",
                    Option ['i'] ["interactive", "repl"] (NoArg RunRepl) "launch REPL" ]

