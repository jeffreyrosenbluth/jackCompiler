module Parser where

import           Text.Megaparsec           hiding (Label)
import           Text.Megaparsec.Text.Lazy (Parser)
import           Text.Megaparsec.Expr

import           Lexer
import           Syntax

parseClass :: Parser Class
parseClass = Class  <$ rword "class"
                   <*> identifier
                    <* symbol "{"
                   <*> sepEndBy classVar semi
                   <*> many subDecl
                    <* symbol "}"

classVar :: Parser ClassVar
classVar = Static  <$ rword "static"
                  <*> parseType
                  <*> sepBy1 identifier comma
       <|> Field   <$ rword "field"
                  <*> parseType
                  <*> sepBy1 identifier comma

parseType :: Parser Type
parseType = IntT     <$ rword "int"
        <|> CharT    <$ rword "char"
        <|> BooleanT <$ rword "boolean"
        <|> ClassT  <$> identifier

parseTypeVoid :: Parser (Maybe Type)
parseTypeVoid = Nothing <$ rword "void"
            <|> optional parseType

subDecl :: Parser SubDecl
subDecl = sd
      <*> parseTypeVoid
      <*> identifier
      <*> parens (sepBy ((,) <$> parseType <*> identifier) comma)
      <*  symbol "{"
      <*> sepEndBy var semi
      <*> many statement
       <* symbol "}"
  where
    sd  = Constructor <$ rword "constructor"
      <|> Function    <$ rword "function"
      <|> Method      <$ rword "method"

var :: Parser Var
var = Var <$ rword "var" <*> parseType <*> sepBy1 identifier comma

statement :: Parser Statement
statement = Let     <$ rword "let"
                   <*> identifier
                   <*> optional (brackets expression)
                    <* symbol "="
                   <*> expression
                    <* semi
        <|> If      <$ rword "if"
                   <*> parens expression
                   <*> braces (many statement)
                   <*> optional (rword "else" *> braces (many statement))
        <|> While   <$ rword "while"
                   <*> parens expression
                   <*> braces (many statement)
        <|> Do      <$ rword "do"
                   <*> subCall
                    <* semi
        <|> Return  <$ rword "return"
                   <*> optional expression
                    <* semi

subCall :: Parser SubCall
subCall = SubCall <$> subName <*> parens (sepBy expression comma)

operators :: [[Operator Parser Expression]]
operators =
  [ [Prefix  (UnOpE Neg      <$ symbol "-") ]
  , [Prefix  (UnOpE Not      <$ symbol "~") ]
  , [ InfixL (BinOpE Times   <$ symbol "*")
    , InfixL (BinOpE Divide  <$ symbol "/") ]
  , [ InfixL (BinOpE And     <$ symbol "&")
    , InfixL (BinOpE Or      <$ symbol "|") ]
  , [ InfixL (BinOpE Plus    <$ symbol "+")
    , InfixL (BinOpE Minus   <$ symbol "-") ]
  , [ InfixL (BinOpE Greater <$ symbol ">")
    , InfixL (BinOpE Less    <$ symbol "<")
    , InfixL (BinOpE Equal   <$ symbol "=")
    ]
  ]

term :: Parser Expression
term = parens expression
   <|> try (CallE <$> subCall)
   <|> ConstantE <$> constant
   <|> VarE <$> identifier <*> optional (brackets expression)

constant :: Parser Constant
constant = IntC . fromInteger <$> integer
       <|> KeywordC <$> keyword
       <|> StringC <$> between (char '"') (char '"') (many (noneOf ['"']))

keyword :: Parser Keyword
keyword = Yes  <$ rword "true"
     <|>  No   <$ rword "false"
     <|>  Null <$ rword "null"
     <|>  This <$ rword "this"


expression :: Parser Expression
expression = makeExprParser term operators
