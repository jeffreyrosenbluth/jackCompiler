
module Lexer  where

import           Control.Applicative
import           Control.Monad             (void)
import           Data.Char
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer     as L
import           Text.Megaparsec.Text.Lazy

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

dot :: Parser String
dot = symbol "."

comma :: Parser String
comma = symbol ","

semi :: Parser String
semi = symbol ";"

plus :: Parser String
plus = symbol "+"

minus :: Parser String
minus = symbol "-"

times :: Parser String
times = symbol "*"

divide :: Parser String
divide = symbol "/"

and :: Parser String
and = symbol "&"

or :: Parser String
or = symbol "|"

lessThan :: Parser String
lessThan = symbol "<"

greaterThan :: Parser String
greaterThan = symbol ">"

equal :: Parser String
equal = symbol "="

not :: Parser String
not = symbol "~"

quote :: Parser String
quote = symbol "\""

integer :: Parser Integer
integer = lexeme L.integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

contents :: Parser a -> Parser a
contents p = sc *> p <* eof

identChars :: Char -> Bool
identChars c = isAlphaNum c || c == '_'

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws  :: [String]
rws =
  [ "class"
  , "constructor"
  , "function"
  , "method"
  , "field"
  , "static"
  , "var"
  , "int"
  , "char"
  , "boolean"
  , "void"
  , "true"
  , "false"
  , "null"
  , "this"
  , "let"
  , "do"
  , "if"
  , "else"
  , "while"
  , "return"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (satisfy identChars)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

subName :: Parser String
subName = try (f <$> identifier <*> char '.' <*> identifier)
      <|> identifier
  where
    f x y z = x ++ (y : z)
