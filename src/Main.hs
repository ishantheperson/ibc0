{-# LANGUAGE MultiWayIf #-}
import ParseIt (maybeGetProgram)
import CodegenBC0 (codegen)

import System.Environment (getArgs)
import System.Console.GetOpt 
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)

import Control.Monad (when)

data Flag = Help | DumpAst | DumpState deriving (Show, Eq) 
main :: IO () 
main = do 
  args <- getArgs 

  case getOpt Permute options args of 
    (opts, files, []) -> 
      if | Help `elem` opts -> printHelp "Simple BC0 compiler" 
         | null files -> do printHelp "Error: missing filenames"
                            exitFailure

         | otherwise -> do mapM_ (compileFile opts) files 

    (_, _, errors) -> printHelp $ concat errors

  where printHelp e = putStrLn e >> putStr (usageInfo "usage: ./ibc0 (flags) (filenames). By Ishan Bhargava" options)

        options = [ Option ['h', '?'] ["help"] (NoArg Help) "print this help message",
                    Option ['t'] ["dump-ast"] (NoArg DumpAst) "prints out the AST",
                    Option ['s'] ["dump-state"] (NoArg DumpState) "prints out the final codegen state" ]

        compileFile :: [Flag] -> FilePath -> IO () 
        compileFile flags fileName = do ast <- maybeGetProgram <$> readFile fileName  
                                        case ast of 
                                          Left parseError -> do putStrLn $ "Parsing '" ++ fileName ++ "' failed."
                                                                print parseError 
                                          Right ast -> do when (DumpAst `elem` flags) (print ast)
                                                          let code = codegen ast 
                                                              outfile = replaceExtension fileName "bc0"
                                                          writeFile outfile code 
                                                          
                                                          
