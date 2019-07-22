{-# LANGUAGE LambdaCase, TemplateHaskell, Rank2Types, TupleSections #-} 
module CodegenBC0 (compileFile,  
                   getBytecode, codegen) where 

import Util 
import ParseIt

import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import Text.Printf (printf)

import Control.Lens
import Control.Arrow ((>>>))
import Control.Monad.State 

import System.FilePath (splitExtension)

type IntPool = [Integer]  
type VariableEnv = [(String, ExpressionType)] -- use elemIndex to get positions
type StringPool = [String] -- strings arent interned, this is an array of individual bytes 
                           -- e.g. ["01", "23", "45", "56", "00"]

data ExpressionType = IntExp | StringExp deriving (Eq, Show) 
-- | Represents internal code generator state
data CodegenState = CodegenState { _intPool :: IntPool, _variables :: VariableEnv, _stringPool :: StringPool } 
makeLenses ''CodegenState 
emptyState = CodegenState [] [] [] 

{- C0VM bytecode format
   Header (magic sequence, version #)
   2 bytes - int pool count
   int pool 

   2 bytes - string pool count
   string pool 

   2 bytes - function pool count (including main)

   for each function:
   2 bytes - num args (main takes 0)
   2 bytes - num local vars
   function length in bytes 

   2 bytes - native pool count 
   native pool -}

data Bytecode = IAdd | ISub | IMul | IDiv | IRem
              | IAnd | IOr | IXor

              | VLoad Int | VStore Int 
              | Bipush Int | Ildc Int | Aldc Int 

              | Goto Int 
              | IfCmpNeq Int | IfCmpEq Int 
              | IfCmpLt Int | IfCmpGt Int 
              | IfCmpLe Int | IfCmpGe Int 

              | InvokeNative NativeFunction 
              | Pop | Return 

              -- Not an actual bytecode, but can be inserted into a [Bytecode] 
              -- list to cause a comment line to show up in the generated code
              | Comment String 
                  deriving Show 
{-
data CodegenError = UnknownVariable String -- TODO: add error handling through writerT monad transformer
instance CompilationError CodegenError where 
  getStage = const "Codegeneration"
-}

-- | Compiles a source file to bytecode
-- | The output file is the same as the input file name, except 
-- | with .bc0 
compileFile :: FilePath -> IO () 
compileFile file = 
  let outputFile = (fst $ splitExtension file) ++ ".bc0"
  in getBytecode <$> readFile file >>= writeFile outputFile

-- | Parses input string and generates bytecode
getBytecode :: String -> String 
getBytecode = codegen . getProgram 

-- | Generates bytecode string from an AST node
codegen :: Statement -> String 
codegen p = let (bytecode, pools) = runState (codegenStatement p) emptyState 
                intPoolBytes = codegenIntPool (view intPool pools)
                stringPoolBytes = codegenStringPool (view stringPool pools)
                mainBytes = codegenMain (view variables pools) bytecode

            in unlines [header, 
                        intPoolBytes, 
                        stringPoolBytes, 
                        "00 01 # function count", 
                        mainBytes, 
                        footer] 

type CodegenBuilder a = a -> State CodegenState [Bytecode]

codegenStatement :: CodegenBuilder Statement 
codegenStatement = \case 
  Sequence statements -> concat <$> traverse codegenStatement statements 
  Print e -> do (expressionCode, expressionType) <- codegenExpression e 
                return $ expressionCode ++ (case expressionType of IntExp -> printIntBytecode
                                                                   StringExp -> printBytecode)

  Assign (varName, declType) e -> do (expressionCode, expressionType) <- codegenExpression e  
                                     instruction <- updatePool (varName, expressionType) variables (pure . VStore)
                                     -- TODO: don't ignore declType
                                     return $ expressionCode ++ instruction

  If test ifBody elseBody -> do (testCode, testType) <- codegenExpression test 
                                when (testType /= IntExp) (errorWithoutStackTrace "Integer expression required")

                                ifBodyCode <- codegenStatement ifBody 
                                elseBodyCode <- codegenStatement elseBody 

                                return $ testCode ++ [Bipush 0, IfCmpNeq 6, 
                                                      Goto (3 + bytecodeLength ifBodyCode)] 
                                                  ++ ifBodyCode 
                                                  ++ [Goto (3 + bytecodeLength elseBodyCode)]
                                                  ++ elseBodyCode 
                                                  
  While test body -> do (testCode, testType) <- codegenExpression test 
                        when (testType /= IntExp) (errorWithoutStackTrace "Integer expression required")

                        bodyCode <- codegenStatement body 

                        let code = [Comment "Loop test"] ++ testCode 
                                                         ++ [Bipush 0, IfCmpNeq 6, 
                                                             Goto (6 + bytecodeLength bodyCode), 
                                                             Comment "Loop body"]
                                                         ++ bodyCode
                        return $ code ++ [Goto $ negate (bytecodeLength code), Comment "Loop end"]

  unsupported -> error $ "Unsupported operation (for now): " ++ show unsupported

  where printIntBytecode = [InvokeNative StringFromInt] ++ printBytecode 
        printBytecode = [InvokeNative NativePrint, Pop] 

codegenExpression :: Expression -> State CodegenState ([Bytecode], ExpressionType)
codegenExpression = \case 
  IntConstant i -> if -128 < i && i < 127 
                     then return ([Bipush $ fromInteger i], IntExp)
                     else updatePool i intPool (pure . Ildc) >>= return . (,IntExp)

  StringLiteral str -> codegenString str >>= return . (,StringExp)
  Identifier name -> do variablesEnv <- view variables <$> get 
                        let (index, variableType) = fromMaybe (errorWithoutStackTrace $ "unknown variable: " ++ name) (lookupElemIndex name variablesEnv)
                        return ([VLoad index], variableType)

  FunctionCall funcName funcArgs -> error "TODO: function calls"
  UnaryOp Negate (IntConstant i) -> codegenExpression $ IntConstant (-i)
  UnaryOp operator operand -> do (operandCode, operandType) <- codegenExpression operand 
                                 when (operandType /= IntExp) (errorWithoutStackTrace "negation or ~ applied to non-integer expression") 
                                 return . (,IntExp) . (operandCode++) $ case operator of 
                                                                          BitNot -> [Bipush (-1), IXor]
                                                                          Negate -> [Bipush (-1), IMul] 

  BinOp operator lhs rhs -> do (lhsCode, lhsType) <- codegenExpression lhs 
                               (rhsCode, rhsType) <- codegenExpression rhs 
                               
                               -- when (not $ validateType lhs rhs operator) (error $ "Invalid types")
                               case (operator, lhsType, rhsType) of 
                                 (NumericOp op, IntExp, IntExp) -> return (lhsCode ++ rhsCode ++ [numericOpMap op], IntExp)
                                 (ComparisonOp op, IntExp, IntExp) -> return (lhsCode ++ rhsCode ++ comparisonJumpCode op, IntExp)
                                 (ComparisonOp op, StringExp, StringExp) -> return (lhsCode ++ rhsCode ++ 
                                                                                    [InvokeNative StringCompare, Bipush 0] ++ 
                                                                                    comparisonJumpCode op, IntExp)
                                
                                 (Plus, IntExp, IntExp) -> return (lhsCode ++ rhsCode ++ [IAdd], IntExp)
                                 (Plus, StringExp, IntExp) -> return (lhsCode ++ rhsCode ++ [InvokeNative StringFromInt, InvokeNative StringJoin], StringExp)
                                 (Plus, IntExp, StringExp) -> return (lhsCode ++ [InvokeNative StringFromInt] ++ rhsCode ++ [InvokeNative StringJoin], StringExp)
                                 (Plus, StringExp, StringExp) -> return (lhsCode ++ rhsCode ++ [InvokeNative StringJoin], StringExp)
                                 _ -> error $ "Type mismatch in expression: " ++ show (BinOp operator lhs rhs)

  where numericOpMap = \case 
          Minus -> ISub 
          Multiply -> IMul 
          Divide -> IDiv 
          Mod -> IRem 
          And -> IAnd
          Or -> IOr 

        numericComparisonMap = \case
          Equal -> IfCmpEq 
          NotEqual -> IfCmpNeq
          Less -> IfCmpLt 
          Greater -> IfCmpGt 
          LessEqual -> IfCmpLe 
          GreaterEqual -> IfCmpGe 

        comparisonJumpCode op = [numericComparisonMap op 8, Bipush 0, Goto 5, Bipush 1]

codegenString :: CodegenBuilder String 
codegenString string = do pool <- view stringPool <$> get 
                          let pos = length pool
                              byteString = map (ubyteToHex . ord) string 
                          modify (over stringPool (++(byteString ++ ["00"]))) 
                          return [Comment $ "load string: " ++ show string,
                                  Aldc pos]

codegenIntPool :: IntPool -> String 
codegenIntPool intPool = let lengthBytes = ushortToHex $ length intPool
                             intBytes = intToHex . fromInteger <$> intPool 
                         in lengthBytes ++ " # int pool count\n" ++ unwords intBytes 

codegenStringPool :: StringPool -> String 
codegenStringPool stringPool = let lengthBytes = ushortToHex $ length stringPool 
                                   stringBytes = concatMap (++" ") stringPool 
                               in unlines [lengthBytes, stringBytes]

codegenMain :: VariableEnv -> [Bytecode] -> String 
codegenMain vars program = unlines ["00 00 # (main) num. args", 
                                    ushortToHex (length vars) ++ " # num. vars", 
                                    ushortToHex (bytecodeLength programWithReturn) ++ " # code length",
                                    concatMap bytecodeMap programWithReturn]
  where programWithReturn = program ++ [Bipush 0, Return]

bytecodeMap :: Bytecode -> String 
bytecodeMap b = (\case 
  Comment _ -> "" -- wow so clever   
  Pop -> "57"

  IAdd -> "60"
  ISub -> "64"
  IMul -> "68"
  IDiv -> "6C"
  IRem -> "70"

  IAnd -> "7E" 
  IOr -> "80" 
  IXor -> "82"

  VLoad i -> "15 " ++ sbyteToHex i
  VStore i -> "36 " ++ sbyteToHex i 

  Bipush i -> "10 " ++ sbyteToHex i 
  Ildc i -> "13 " ++ ushortToHex i 
  Aldc i -> "14 " ++ ushortToHex i 

  Goto i -> "A7 " ++ sshortToHex i 
  IfCmpEq i -> "9F " ++ sshortToHex i
  IfCmpNeq i -> "A0 " ++ sshortToHex i 
  IfCmpLt i -> "A1 " ++ sshortToHex i 
  IfCmpGt i -> "A3 " ++ sshortToHex i 
  IfCmpLe i -> "A4" ++ sshortToHex i 
  IfCmpGe i -> "A2" ++ sshortToHex i 

  InvokeNative func -> "B7 " ++ ushortToHex (fromEnum func) 
  Return -> "B0") b ++ "       \t# " ++ showBytecode b ++ "\n" 

showBytecode :: Bytecode -> String  
showBytecode = \case 
  Comment s -> s 
  other -> show other 

bytecodeLength :: [Bytecode] -> Int 
bytecodeLength = sum . map bytecodeArity

-- Requires rank 2 types for Lens'  
updatePool :: Eq a => a -> Lens' b [a] -> (Int -> c) -> State b c 
updatePool elem lens f = do pool <- view lens <$> get 
                            case elemIndex elem pool of 
                              Just index -> return $ f index 
                              Nothing -> do modify (over lens (++[elem]))
                                            return . f $ length pool 

bytecodeArity :: Bytecode -> Int 
bytecodeArity = \case 
  Ildc _ -> 3 
  Aldc _ -> 3
 
  InvokeNative _ -> 3 
 
  Goto _ -> 3
  IfCmpEq _ -> 3
  IfCmpNeq _ -> 3 
  IfCmpLt _ -> 3
  IfCmpGt _ -> 3 
  IfCmpLe _ -> 3 
  IfCmpGe _ -> 3 

  VLoad _ -> 2
  VStore _ -> 2 
  Bipush _ -> 2

  Comment _ -> 0 

  _ -> 1 -- most instructions do not take operands 

data NativeFunction = StringFromInt 
                    | StringJoin
                    | StringCompare
                    | NativePrint 
                        deriving (Show, Enum, Bounded)
-- This should be loaded from a file instead 
-- Maybe parsing c0_c0ffi.h 
nativeFuncMap exp = let code = case exp of StringFromInt -> "00 01 00 63"
                                           StringJoin -> "00 02 00 64"
                                           StringCompare -> "00 02 00 5E"
                                           NativePrint -> "00 01 00 0A" -- change 0A to 06 for print instead of println
                    in code ++ " # " ++ show exp 

header, footer :: String 
header = "C0 C0 FF EE 00 13 # header\n" 
footer = let functions = [minBound..maxBound] :: [NativeFunction]
             
             countText = ushortToHex (length functions) ++ " # native pool count"
             functionsText = map nativeFuncMap functions 

         in unlines $ countText:functionsText

sbyteToHex, ubyteToHex, ushortToHex, sshortToHex, intToHex :: Int -> String 
sbyteToHex i = printf "%02X" (if i < 0 then i + (2^8) else i)
ubyteToHex i = printf "%02X" i 

ushortToHex i = addSpaces (printf "%04X" i) 
sshortToHex i = addSpaces (printf "%04X" (if i < 0 then i + (2^16) else i)) 

-- signed 32-bit int to hex
intToHex i = addSpaces (printf "%08X" (if i < 0 then i + (2^32) else i))

-- adds spaces every 2 characters 
-- e.g. 1234 -> 12 34 
addSpaces :: String -> String 
addSpaces = \case 
  [] -> []
  a:[] -> a:[]
  a:b:[] -> a:b:[]
  a:b:xs -> a:b:' ':(addSpaces xs)