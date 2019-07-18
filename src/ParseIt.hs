{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-missing-signatures #-}
{-# LANGUAGE BlockArguments #-}
module ParseIt (BoolExpr(..), BoolOp(..), CmpOp(..), 
                ArithExpr(..), ArithOp(..), 
                Statement(..), Expression(..),
                maybeGetProgram, getProgram) where 

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Tok 

data BoolExpr = BoolConst Bool 
              | Not BoolExpr
              | BoolBinary BoolOp BoolExpr BoolExpr
              | CmpBinary CmpOp ArithExpr ArithExpr
                  deriving Show 

data BoolOp = And | Or deriving (Show, Eq) 
data CmpOp = Less | LessEqual | Greater | GreaterEqual 
           | Equal | NotEqual deriving (Show, Eq) 

data ArithExpr = IntConstant Integer  
               | Variable String 
               | Negate ArithExpr 
               | ArithBinary ArithOp ArithExpr ArithExpr

data ArithOp = Add | Subtract | Multiply | Divide | Mod deriving Show 
data Statement = Sequence [Statement] 
               | Assign String Expression 
               | If BoolExpr Statement Statement 
               | While BoolExpr Statement 
               | Print Expression
               | FunctionDecl String [String] Statement
               | FunctionReturn Expression  
                   deriving Show 

data Expression = BoolExpr BoolExpr | ArithExpr ArithExpr | StringLiteral String 
                | FunctionCall String [Expression] 
                    deriving Show

instance Show ArithExpr where 
  show (IntConstant i) = show i 
  show (Variable v) = v 
  show (Negate e) = "-(" ++ show e ++ ")"
  show (ArithBinary op lhs rhs) = let lhsText = show lhs 
                                      rhsText = show rhs 
                                      opText = case op of Add -> "+"
                                                          Subtract -> "-"
                                                          Multiply -> "*"
                                                          Divide -> "/"
                                                          Mod -> "%"
                                      in unwords [lhsText, opText, rhsText]

reservedWords = [ "if", "else", "while", 
                  "true", "false",
                  "print" ]

reservedOps = [ "+", "-", "*", "/", "==", "!=", "!", "&&", "||",
                "+=", "*=", "-=", "/=", "%=", "=", "=>" ]

languageDef = emptyDef { 
  Tok.commentStart = "/*",
  Tok.commentEnd = "*/",
  Tok.commentLine = "//",
  Tok.identStart = letter, 
  Tok.identLetter = alphaNum,
  Tok.reservedNames = reservedWords,
  Tok.reservedOpNames = reservedOps,
  Tok.opLetter = oneOf "+-*/=!&|%"
}

lexer = Tok.makeTokenParser languageDef 

identifier = Tok.identifier lexer 
reserved = Tok.reserved lexer 
reservedOp = Tok.reservedOp lexer 
parens = Tok.parens lexer 
braces = Tok.braces lexer 
integer = Tok.integer lexer 
semicolon = Tok.semi lexer 
whitespace = Tok.whiteSpace lexer
stringLiteral = Tok.stringLiteral lexer 
commaSep = Tok.commaSep lexer 

parseProgram :: Parser Statement 
parseProgram = do 
  whitespace 
  program <- parseSequence 
  eof
  return program 

parseSequence :: Parser Statement 
parseSequence = do 
  statementList <- many parseStatement 
  return $ case statementList of 
            [s] -> s 
            _ -> Sequence statementList 

parseStatement :: Parser Statement 
parseStatement =     parseIf 
                 <|> parseWhile
                 <|> parseFunctionDecl
                 <|> parseAssign 
                 <|> parsePrint 
                 <?> "statement/declaration"

parseExpression =    parseFunctionCall
                 <|> (try $ ArithExpr <$> parseArithExpr) 
                 <|> (try $ BoolExpr <$> parseBoolExpr)
                 <|> (StringLiteral <$> stringLiteral)
                 <?> "expression"

parseIf = do 
  reserved "if"
  condition <- parens parseBoolExpr
  
  ifBody <- braces parseSequence 
  elseBody <- option (Sequence []) (reserved "else" *> braces parseSequence) 

  return $ If condition ifBody elseBody

parseWhile = do 
  reserved "while" 
  condition <- parens parseBoolExpr 
  loopBody <- braces parseSequence 

  return $ While condition loopBody 

parseFunctionDecl = do 
  (name, args) <- try do 
    name <- identifier 
    args <- parens $ commaSep identifier
    return (name, args) 

  body <- parseSingleStatementFunction <|> parseMultiStatementFunction 
  return $ FunctionDecl name args body 

  where parseSingleStatementFunction = do 
          reservedOp "=>"
          FunctionReturn <$> parseExpression <* reservedOp ";"

        parseMultiStatementFunction = braces $ parseSequence 

parseFunctionCall = do 
  (name, args) <- try do 
    name <- identifier 
    args <- parens $ commaSep parseExpression 
    return (name, args)

  return $ FunctionCall name args 

parseAssign = do 
  name <- identifier 
  op <- choice (map (\s -> reservedOp s >> return s) ["=", "+=", "-=", "*=", "/="])
  expr <- parseExpression 
  semicolon 
  return $ case op of 
             "=" -> Assign name expr 
             c:"=" -> let operation = 
                               case c of 
                                 '+' -> Add
                                 '-' -> Subtract 
                                 '*' -> Multiply 
                                 '/' -> Divide 
                                 _ -> error "(supposedly impossible) parser error"
                          expr' = 
                               case expr of -- FIXME: Can be avoided by creating new 
                                 ArithExpr e -> e  -- AST node type for compound assignment
                                 _ -> error "expected ArithExpr"
                      in Assign name (ArithExpr $ ArithBinary operation (Variable name) expr') 
             _ -> error "(supposedly impossible) parser error" 

parsePrint = do 
  reserved "print" 
  Print <$> parseExpression <* semicolon

parseArithExpr :: Parser ArithExpr 
parseArithExpr = buildExpressionParser arithOperators arithTerm 

parseBoolExpr :: Parser BoolExpr
parseBoolExpr = buildExpressionParser boolOperators boolTerm 

arithOperators = [[Prefix (reservedOp "-" >> return (Negate))],
                  [Infix (reservedOp "*" >> return (ArithBinary Multiply)) AssocLeft,
                   Infix (reservedOp "/" >> return (ArithBinary Divide)) AssocLeft,
                   Infix (reservedOp "%" >> return (ArithBinary Mod)) AssocLeft],
                  [Infix (reservedOp "+" >> return (ArithBinary Add)) AssocLeft,
                   Infix (reservedOp "-" >> return (ArithBinary Subtract)) AssocLeft]]

boolOperators = [[Prefix (reservedOp "!" >> return (Not))],
                 [Infix (reservedOp "||" >> return (BoolBinary Or)) AssocLeft],
                 [Infix (reservedOp "&&" >> return (BoolBinary And)) AssocLeft]]

arithTerm =     parens parseArithExpr 
            <|> (Variable <$> identifier) 
            <|> (IntConstant <$> integer)
            <?> "arithmetic expression"

boolTerm =     parens parseBoolExpr 
           <|> parseCmpExpression 
           <|> (reserved "true" >> return (BoolConst True))
           <|> (reserved "false" >> return (BoolConst False))
           <?> "boolean expression"

parseCmpExpression = do 
  lhs <- parseArithExpr 
  op <- parseRelation 
  rhs <- parseArithExpr 

  return $ CmpBinary op lhs rhs 

parseRelation =     (reservedOp "==" >> return Equal)
                <|> (reservedOp "!=" >> return NotEqual) 
                <|> (reservedOp "<" >> return Less)
                <|> (reservedOp "<=" >> return LessEqual)
                <|> (reservedOp ">" >> return Greater)
                <|> (reservedOp ">=" >> return GreaterEqual)
                <?> "comparison operator"

maybeGetProgram :: String -> Either ParseError Statement
maybeGetProgram = parse parseProgram "" 

getProgram :: String -> Statement
getProgram str = 
  case maybeGetProgram str of 
    Left e -> error $ show e 
    Right program -> program 
