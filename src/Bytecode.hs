{-# LANGUAGE LambdaCase #-}
module Bytecode where 

import Data.Char (toLower)

data Bytecode = IAdd | ISub | IMul | IDiv | IRem
              | IAnd | IOr | IXor

              | VLoad Int | VStore Int 
              | Bipush Int | Ildc Int | Aldc Int 

              | Goto Int 
              | IfCmpNeq Int | IfCmpEq Int 
              | IfCmpLt Int | IfCmpGt Int 
              | IfCmpLe Int | IfCmpGe Int 

              | NewArray Int 
              | AAdds
              | IMLoad | IMStore 
              | AMLoad | AMStore 

              | InvokeNative NativeFunction 
              | Return 

              | Pop | Dup 

              -- | Not an actual bytecode, but can be inserted into a [Bytecode] 
              -- | list to cause a comment line to show up in the generated code
              | Comment String 
                  deriving Show 

data NativeFunction = StringFromInt 
                    | StringJoin
                    | StringCompare
                    | NativePrint 
                        deriving (Enum, Bounded, Show)


showBytecode :: Bytecode -> String  
showBytecode = \case 
  Comment s -> s 
  other -> map toLower $ show other 

bytecodeLength :: [Bytecode] -> Int 
bytecodeLength = sum . map bytecodeArity

-- FIXME: merge this with bytecodeMap 
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

  NewArray _ -> 2 -- FIXME: veritfy this value 

  Comment _ -> 0 

  _ -> 1 -- most instructions do not take operands 
