{-# LANGUAGE MultiWayIf #-}
import CodegenBC0 (compileFile)

import System.Environment (getArgs)
import System.Console.GetOpt 
import System.Exit (exitFailure)

data Flag = GenBytecode | Help deriving (Show, Eq) 

main :: IO () 
main = do 
  args <- getArgs 

  case getOpt Permute options args of 
    (opts, files, []) -> 
      if | Help `elem` opts -> printHelp "Simple BC0 compiler" 
         | null files -> do printHelp "Error: missing filenames"
                            exitFailure

         | GenBytecode `elem` opts -> mapM_ compileFile files 
         | otherwise -> do putStrLn "Error: The -b is currently required"
                           exitFailure

    (_, _, errors) -> printHelp $ concat errors

  where printHelp e = putStrLn e >> putStr (usageInfo "usage: ./ibc0 (flags) (filenames). By Ishan Bhargava" options)

        options = [ Option ['h', '?'] ["help"] (NoArg Help) "print this help message",
                    Option ['b'] ["bytecode"] (NoArg GenBytecode) "generate BC0" ]

