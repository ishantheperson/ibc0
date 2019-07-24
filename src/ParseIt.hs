{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-missing-signatures #-}
{-# LANGUAGE BlockArguments #-}
module ParseIt (Statement(..), Expression(..), LValue(..), VariableDecl,
                NumericOperator(..), ComparisonOperator(..), BinOperator(..), UnaryOperator(..),
                maybeGetProgram, getProgram) where 

import Util 

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Tok 

import Data.Either (either)
import Control.Arrow ((>>>))

instance CompilationError ParseError where 
  getStage = const "Parsing" 

-- | Represents a variable name with a type 
type VariableDecl = (String, String)
data Statement = Sequence [Statement] 
               | Assign LValue Expression 
               | If Expression Statement Statement 
               | While Expression Statement 
               | Print Expression
               | FunctionDecl String String [VariableDecl] Statement
               | FunctionReturn Expression  
               | FunctionCallStatement Expression 
                   deriving Show 

data LValue = VariableL VariableDecl 
            | ArrayL String Expression 
                deriving Show

data Expression = -- Terms
                  IntConstant Integer | StringLiteral String | Identifier String 
                | FunctionCall String [Expression]
                | ArrayLiteral [Expression]
                | ArrayAccess Expression Expression 
                  -- Expression parsers 
                | BinOp BinOperator Expression Expression 
                | UnaryOp UnaryOperator Expression 
                    deriving Show 

data BinOperator = Plus | NumericOp NumericOperator | ComparisonOp ComparisonOperator deriving Show 
data NumericOperator = Minus | Multiply | Mod | Divide | And | Or deriving Show 
data ComparisonOperator = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual deriving Show 
data UnaryOperator = Negate | BitNot deriving Show 

-- Public API 
-- | Returns either one parser error or an AST
maybeGetProgram :: String -> Either ParseError Statement
maybeGetProgram = parse parseProgram "" 

-- | Either returns an AST or crashes w/ syntax error
getProgram :: String -> Statement
getProgram = maybeGetProgram >>> either (error . show) id 

-- Private 

parseProgram, parseSequence, parseStatement :: Parser Statement 
parseProgram = do 
  whitespace 
  program <- parseSequence 
  eof
  return program 
 
parseSequence = do 
  statementList <- many parseStatement 
  return $ case statementList of 
            [s] -> s 
            _ -> Sequence statementList 

parseStatement =     parseIf 
                 <|> parseWhile
                 <|> FunctionCallStatement <$> try (parseFunctionCall <* semicolon)
                 <|> parseFunctionDecl
                 <|> parseAssign 
                 <|> parsePrint 
                 <|> parseReturn 
                 <?> "statement/declaration"

parseIf, parseWhile, parseFunctionDecl, parseAssign, parsePrint, parseReturn :: Parser Statement 

parseIf = do 
  reserved "if"
  condition <- parens parseExpression
  
  ifBody <- braces parseSequence 
  elseBody <- option (Sequence []) (reserved "else" *> braces parseSequence) 

  return $ If condition ifBody elseBody

parseWhile = do 
  reserved "while" 
  condition <- parens parseExpression 
  loopBody <- braces parseSequence 

  return $ While condition loopBody 

parseFunctionDecl = do 
  (name, args) <- try do 
    name <- identifier 
    args <- parens $ commaSep parseVariableDecl
    return (name, args) 

  declType <- parseTypeAnnotation 
  body <- parseSingleStatementFunction <|> parseMultiStatementFunction 
  return $ FunctionDecl name declType args body 

  where parseSingleStatementFunction = do 
          reservedOp "=>"
          FunctionReturn <$> parseExpression <* reservedOp ";"

        parseMultiStatementFunction = braces parseSequence 

parseAssign = do 
  name <- parseLvalue 
  reservedOp "="
  expr <- parseExpression 
  semicolon 
  return $ Assign name expr 

parsePrint = do 
  reserved "print" 
  Print <$> parseExpression <* semicolon

parseReturn = do 
  reserved "return"
  FunctionReturn <$> parseExpression <* semicolon

parseLvalue :: Parser LValue 
parseLvalue =  try parseArrayL 
           <|> VariableL <$> parseVariableDecl

parseArrayL = do 
  name <- identifier 
  pos <- brackets parseExpression 

  return $ ArrayL name pos 

parseExpression :: Parser Expression 
parseExpression = buildExpressionParser operators parseTerm <?> "expression"

parseTerm =    parens parseExpression 
           <|> ArrayLiteral <$> parseArrayLiteral
           <|> parseFunctionCall
           <|> Identifier <$> identifier 
           <|> IntConstant<$> integer 
           <|> StringLiteral <$> stringLiteral
           <|> (reserved "true" >> return (IntConstant 1))
           <|> (reserved "false" >> return (IntConstant 0))
           -- <?> "expression"

parseArrayLiteral = brackets $ commaSep parseExpression 

parseFunctionCall = try do  
  name <- identifier 
  args <- parens $ commaSep parseExpression  

  return $ FunctionCall name args 

parseVariableDecl = do 
  name <- identifier 
  declType <- parseTypeAnnotation 

  return (name, declType)

-- | Parses a type annotation. Defaults to 'int'
parseTypeAnnotation = option "int" (reservedOp ":" *> identifier)

-- | Operators in order from highest precedence to lowest 
operators = [[Postfix (flip ArrayAccess <$> brackets parseExpression)],
             [Prefix (reservedOp "-" >> return (UnaryOp Negate)),
              Prefix (reservedOp "~" >> return (UnaryOp BitNot)),
              Prefix (reservedOp "!" >> return (UnaryOp BitNot))],

             [makeOp "*" $ NumericOp Multiply,
              makeOp "/" $ NumericOp Divide,
              makeOp "%" $ NumericOp Mod],

             [makeOp "+"   Plus,
              makeOp "-" $ NumericOp Minus],

             [makeOp "<"  $ ComparisonOp Less,
              makeOp "<=" $ ComparisonOp LessEqual,
              makeOp ">"  $ ComparisonOp Greater,
              makeOp ">=" $ ComparisonOp GreaterEqual],
 
             [makeOp "==" $ ComparisonOp Equal,
              makeOp "!=" $ ComparisonOp NotEqual],
 
             [makeOp "&&" $ NumericOp And],
             [makeOp "||" $ NumericOp Or]]

  where makeOp s f = Infix (reservedOp s >> return (BinOp f)) AssocLeft                                


reservedWords = [ "if", "else", "while", 
                  "true", "false",
                  "print", "return" ]

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
Tok.opLetter = oneOf "+-*/=!&|%<>"
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
brackets = Tok.brackets lexer 
