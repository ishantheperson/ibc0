{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-missing-signatures #-}
{-# LANGUAGE BlockArguments #-}
module ParseIt (maybeGetProgram, getProgram) where 

import Util 
import AST 

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Tok 

import Data.Either (either)
import Control.Arrow ((>>>))

instance CompilationError ParseError where 
  getStage = const "Parsing" 

-- Public API 
-- | Returns either one error or an AST
maybeGetProgram :: String -> Either ParseError Statement
maybeGetProgram = parse program "" 

-- | Either returns an AST or crashes w/ syntax error
getProgram :: String -> Statement
getProgram = maybeGetProgram >>> either (errorWithoutStackTrace . show) id 

-- Private 

program, sequenceStmnts, statement :: Parser Statement 
program = do 
  whitespace 
  program <- sequenceStmnts
  eof
  return program 
 
-- FIXME: is it still necessary to extract singleton statements 
sequenceStmnts = do 
  statementList <- many statement 
  return $ case statementList of 
            [s] -> s 
            _ -> Sequence statementList 

statement =  ifStmnt 
         <|> whileLoop
         <|> FunctionCallStatement <$> try (functionCall <* semicolon)
         <|> functionDecl
         <|> assign 
         <|> printStmnt 
         <|> returnStmnt 
         <?> "statement/declaration"

ifStmnt, whileLoop, functionDecl, assign, printStmnt, returnStmnt :: Parser Statement 
ifStmnt = do 
  reserved "if"
  condition <- parens expression
  
  ifBody <- braces sequenceStmnts 
  elseBody <- option (Sequence []) (reserved "else" *> braces sequenceStmnts) 

  return $ If condition ifBody elseBody

whileLoop = do 
  reserved "while" 
  condition <- parens expression 
  loopBody <- braces sequenceStmnts 

  return $ While condition loopBody 

functionDecl = do 
  (name, args) <- try do 
    name <- identifier 
    args <- parens $ commaSep variableDecl
    return (name, args) 

  declType <- typeAnnotation 
  body <- singleStatementFunction <|> multiStatementFunction 
  return $ FunctionDecl name declType args body 

  where singleStatementFunction = do 
          reservedOp "=>"
          FunctionReturn <$> expression <* reservedOp ";"

        multiStatementFunction = braces sequenceStmnts

assign = do 
  name <- expression  
  reservedOp "="
  expr <- expression 
  semicolon 
  return $ Assign name expr 

printStmnt = do 
  reserved "print" 
  Print <$> expression <* semicolon

returnStmnt = do 
  reserved "return"
  FunctionReturn <$> expression <* semicolon

{-
lvalue :: Parser LValue 
lvalue = VariableL <$> identifier >>= postfix 
  where postfix e = arrayAccess e <|> return e 
        arrayAccess e = do index <- brackets expression 
                           postfix $ ArrayL e index 
-}
                           
expression, term :: Parser Expression 
expression = buildExpressionParser operators postfix <?> "expression"
  where postfix = term >>= postfix'

        postfix', arrayAccess :: Expression -> Parser Expression 
        postfix' e = arrayAccess e <|> return e 
        arrayAccess e = do index <- brackets expression
                           postfix' $ ArrayAccess e index 

        -- Terser but horribly unreadable 
        -- arrayAccess e = brackets parseExpression >>= parsePostfix' . ArrayAccess e 

term =  parens expression 
    <|> ArrayLiteral <$> arrayLiteral
    <|> functionCall
    <|> Identifier <$> identifier 
    <|> IntConstant<$> integer 
    <|> StringLiteral <$> stringLiteral
    <|> (reserved "true" >> return (IntConstant 1))
    <|> (reserved "false" >> return (IntConstant 0))

arrayLiteral = brackets $ commaSep expression 

functionCall = try do  
  name <- identifier 
  args <- parens $ commaSep expression  

  return $ FunctionCall name args 

{-
variableDecl = do 
  name <- identifier 
  declType <- typeAnnotation 

  return (name, declType)
-}
variableDecl = identifier 
-- | Parses a type annotation. Defaults to 'int'
typeAnnotation = option "int" (reservedOp ":" *> identifier)

-- | Operators in order from highest precedence to lowest 
operators = [[Prefix (reservedOp "-" >> return (UnaryOp Negate)),
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
  Tok.identLetter = alphaNum <|> char '_',
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
