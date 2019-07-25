module AST where 

-- | Represents a variable name with a type 
data Statement = Sequence [Statement] 
               | Assign LValue Expression 
               | If Expression Statement Statement 
               | While Expression Statement 
               | Print Expression
               | FunctionDecl String String [String] Statement
               | FunctionReturn Expression  
               | FunctionCallStatement Expression 
                   deriving Show 

data LValue = VariableL String  
            | ArrayL LValue Expression 
                deriving Show

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
data UnaryOperator = Negate | BitNot deriving Show