{-# LANGUAGE LambdaCase #-}
module AST where 

-- | Represents a variable name with a type 
data Statement = Sequence [Statement] 
               | Assign Expression Expression 
               | If Expression Statement Statement 
               | While Expression Statement 
               | Print Expression
               | FunctionDecl String String [String] Statement
               | FunctionReturn Expression  
               | FunctionCallStatement Expression 
                   deriving Show 

{-
data LValue = VariableL String  
            | ArrayL LValue Expression 
                deriving Show
-}  

data Expression = -- Terms
                  IntConstant Integer | StringLiteral String | Identifier String 
                | FunctionCall String [Expression]
                | ArrayLiteral [Expression]
                | ArrayAccess Expression Expression 
                  -- Expression rs 
                | BinOp BinOperator Expression Expression 
                | UnaryOp UnaryOperator Expression 
                    deriving Show 

data BinOperator = Plus | NumericOp NumericOperator | ComparisonOp ComparisonOperator deriving Show 
data NumericOperator = Minus | Multiply | Mod | Divide | And | Or deriving Show 
data ComparisonOperator = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual deriving Show 
data UnaryOperator = 
    -- | Integer negation e.g. -1
    Negate 
    -- | Bitwise negation e.g. ~0x33
  | BitNot deriving Show

-- TODO: nicer Show instances 
{-
instance Show Expression where 
  show = \case 
    IntConstant i -> show i 
    StringLiteral s -> show s 
    Identifier v -> v 
    ArrayLiteral l -> show l 
    ArrayAccess arr i -> (show arr) ++ "[" ++ show i ++ "]"
    BinOp op lhs rhs -> lhs ++ " " ++ show op ++ " " ++ rhs 
    UnaryOp -> show UnaryOp ++ show Expression
-}

{-
instance Show NumericOperator where 
  show = \case 
    Minus -> "-"
    Multiply -> "*"
    Mod -> "%"
    Divide -> "/"
    And -> "&"
    Or -> "|"

instance Show ComparisonOperator where 
  show = \case 

-}