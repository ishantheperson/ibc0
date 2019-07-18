{-# LANGUAGE LambdaCase #-}
module EvalIt (Environment, emptyEnvironment,
               RuntimeError(..),
               evalStatement, runProgram) where 

import ParseIt 
import Control.Monad.Except 
import qualified Data.Map.Strict as Map 

data RuntimeError = UnknownVar String | DivZero | WrongType

instance Show RuntimeError where 
  show e = "Runtime error: " ++ case e of 
                                  UnknownVar v -> "Unknown variable '" ++ v ++ "'"
                                  DivZero -> "Division by zero" 
                                  WrongType -> "Type mismatch"

-- data Variable = StringVar String | IntVar Integer 
type Environment = Map.Map String Integer 

emptyEnvironment :: Environment
emptyEnvironment = Map.empty 

evalBoolExpr :: Environment -> BoolExpr -> Either RuntimeError Bool 
evalBoolExpr env = \case 
  BoolConst b -> return b
  Not e' -> not <$> evalBoolExpr env e' 
  BoolBinary op b1 b2 -> do b1' <- evalBoolExpr env b1 
                            b2' <- evalBoolExpr env b2 
                            let func = case op of 
                                         And -> (&&)
                                         Or -> (||) 
                            return $ func b1' b2'  

  CmpBinary op a1 a2 -> do a1' <- evalArithExpr env a1 
                           a2' <- evalArithExpr env a2 
                           let func = case op of 
                                        Less -> (<)
                                        LessEqual -> (<=)
                                        Greater -> (>)
                                        GreaterEqual -> (>=)
                                        Equal -> (==)
                                        NotEqual -> (/=)
                           return $ func a1' a2' 

getBoolExpr :: Environment -> Expression -> Either RuntimeError Bool 
getBoolExpr env (BoolExpr e) = evalBoolExpr env e 
getBoolExpr _ _ = Left WrongType 

evalArithExpr :: Environment -> ArithExpr -> Either RuntimeError Integer
evalArithExpr env = \case
  IntConstant i -> return i
  Variable v -> maybe (Left $ UnknownVar v) Right (Map.lookup v env)
   
  Negate e' -> negate <$> evalArithExpr env e' 
  ArithBinary op lhs rhs -> 
    do lhs' <- evalArithExpr env lhs  
       rhs' <- evalArithExpr env rhs 
       func <- case op of 
                 Add -> return (+)
                 Subtract -> return (-)
                 Multiply -> return (*)
                 Divide -> do when (rhs' == 0) (Left DivZero)
                              return div 
                 Mod -> do when (rhs' == 0) (Left DivZero) 
                           return mod 
       return $ func lhs' rhs' 

getArithExpr :: Environment -> Expression -> Either RuntimeError Integer 
getArithExpr env (ArithExpr e) = evalArithExpr env e
getArithExpr _ _ = Left WrongType 

getStringExpr :: Environment -> Expression -> Either RuntimeError String 
getStringExpr _ (StringLiteral s) = Right s 
getStringExpr _ _ = Left WrongType 

evalStatement :: Environment -> Statement -> ExceptT RuntimeError IO Environment 
evalStatement env statement = case statement of  
  Sequence statements -> evalSequence env statements 
  Assign name value -> do newVal <- liftEither $ getArithExpr env value 
                          return $ Map.insert name newVal env 
  
  If cond ifBranch elseBranch -> do test <- liftEither $ evalBoolExpr env cond 
                                    evalStatement env (if test then ifBranch else elseBranch)

  While cond loopBody -> do test <- liftEither $ evalBoolExpr env cond 
                            if test then do 
                              newEnv <- evalStatement env loopBody 
                              evalStatement newEnv statement 
                            else 
                              return env 
  
  Print expr -> do val <- case expr of 
                            ArithExpr aexp -> liftEither . fmap show $ evalArithExpr env aexp 
                            BoolExpr bexp -> liftEither . fmap show $ evalBoolExpr env bexp 
                            StringLiteral s -> pure s 
                   liftIO $ putStrLn val 
                   return env

evalSequence :: Environment -> [Statement] -> ExceptT RuntimeError IO Environment 
evalSequence env statements = 
  foldl (\wrappedEnv' expr -> do env' <- wrappedEnv' 
                                 evalStatement env' expr) 
        (pure env) statements 

runProgram :: Statement -> IO (Either RuntimeError Environment) 
runProgram = runExceptT . evalStatement emptyEnvironment
