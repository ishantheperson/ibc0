module Repl where 
import ParseIt 
import EvalIt 

import Control.Monad.Except
import System.Console.Haskeline 

runRepl :: IO () 
runRepl = runInputT defaultSettings (loop emptyEnvironment) 
  where loop :: Environment -> InputT IO () 
        loop env = do 
          minput <- getInputLine ">>> "
          case minput of 
            Nothing -> outputStrLn "Goodbye"
            Just ":q" -> outputStrLn "Goodbye" 

            Just ":vars" -> do liftIO $ print env 
                               loop env 
            Just ":{" -> do inputLines <- extendedInput 
                            nextEnv <- tryRunProgram env $ unlines inputLines 
                            loop nextEnv
              
            Just input -> do nextEnv <- tryRunProgram env input 
                             loop nextEnv

        extendedInput :: InputT IO [String] 
        -- Reads multiple lines at once 
        -- :{ starts, Ctrl-D or :} ends 
        extendedInput = do 
           minput <- getInputLine "... "
           case minput of 
             Nothing -> return [] 
             Just ":}" -> return [] 
             Just input -> (input:) <$> extendedInput 

        tryRunProgram :: Environment -> String -> InputT IO Environment 
        -- Either prints out an error and returns the old environment,
        -- or returns the new environment 
        tryRunProgram env programString = case maybeGetProgram programString of 
                                Left err -> do outputStrLn $ show err 
                                               return env 
                                Right program -> do result <- liftIO . runExceptT $ evalStatement env program 
                                                    case result of 
                                                      Left err -> do outputStrLn $ show err 
                                                                     return env 
                                                      Right newEnv -> return newEnv 
