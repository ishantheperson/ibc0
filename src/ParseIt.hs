{-# OPTIONS_GHC -Wno-unused-do-bind -Wno-missing-signatures #-}
{-# LANGUAGE BlockArguments #-}
module ParseIt (Statement(..), Expression(..), BinOperator(..), UnaryOperator(..),
                maybeGetProgram, getProgram) where 

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr 
import Text.ParserCombinators.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as Tok 

data Statement = Sequence [Statement] 
               | Assign String Expression 
               | If Expression Statement Statement 
               | While Expression Statement 
               | Print Expression
               | FunctionDecl String [String] Statement
               | FunctionReturn Expression  
               | FunctionCallStatement Expression 
                   deriving Show 

data Expression = -- Terms
                  IntConstant Integer | StringLiteral String | Identifier String 
                | FunctionCall String [Expression]
                  -- Expression parsers 
                | BinOp BinOperator Expression Expression 
                | UnaryOp UnaryOperator Expression 
                    deriving Show 

data BinOperator = Plus | Minus | Multiply | Mod | Divide 
                 | And | Or | Equal | NotEqual 
                 | Less | LessEqual | Greater | GreaterEqual 
                     deriving Show 

data UnaryOperator = Not | Negate | BitNot deriving Show 

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
    args <- parens $ commaSep identifier
    return (name, args) 

  body <- parseSingleStatementFunction <|> parseMultiStatementFunction 
  return $ FunctionDecl name args body 

  where parseSingleStatementFunction = do 
          reservedOp "=>"
          FunctionReturn <$> parseExpression <* reservedOp ";"

        parseMultiStatementFunction = braces $ parseSequence 

parseAssign = do 
  name <- identifier 
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

parseExpression = buildExpressionParser operators parseTerm

operators = [[Prefix (reservedOp "-" >> return (UnaryOp Negate)),
              Prefix (reservedOp "~" >> return (UnaryOp BitNot))],
             [makeOp "*" Multiply,
              makeOp "/" Divide,
              makeOp "%" Mod],
             [makeOp "+" Plus,
              makeOp "-" Minus],
             [makeOp "<" Less,
              makeOp "<=" LessEqual,
              makeOp ">" Greater,
              makeOp ">=" GreaterEqual],
             [makeOp "==" Equal,
              makeOp "!=" NotEqual],
             [makeOp "&&" And],
             [makeOp "||" Or]]

  where makeOp s f = Infix (reservedOp s >> return (BinOp f)) AssocLeft

parseTerm =    parens parseExpression 
           <|> parseFunctionCall
           <|> Identifier <$> identifier 
           <|> IntConstant<$> integer 
           <|> StringLiteral <$> stringLiteral
           <|> (reserved "true" >> return (IntConstant 1))
           <|> (reserved "false" >> return (IntConstant 0))
           -- <?> "expression"

parseFunctionCall = do 
  (name, args) <- try do 
    name <- identifier 
    args <- parens $ commaSep parseExpression 
    return (name, args)

  return $ FunctionCall name args 


maybeGetProgram :: String -> Either ParseError Statement
maybeGetProgram = parse parseProgram "" 

getProgram :: String -> Statement
getProgram str = 
  case maybeGetProgram str of 
    Left e -> error $ show e 
    Right program -> program 
