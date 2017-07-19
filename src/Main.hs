module Main where

import Syntax
import Lexer
import Parser

import           Text.Megaparsec
import           Data.Text.Lazy
import           System.Environment       (getArgs)

main ::IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      p <- parseFile path
      case p of
        Left e -> print e
        Right t -> print t



parseFile :: FilePath -> IO (Either (ParseError Char Dec) Class)
parseFile fp = do
  f <- readFile fp
  return $ parse (contents parseClass) "<stdin>" (pack f)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     []     -> putStrLn "Error - no source file specified."
--     [path] -> do
--       files <- find (depth <? 10) (extension ==? ".vm") path
--       outB <- traverse processFile files
--       let out = T.toLazyText . mconcat $ bootstrp : outB
--       T.writeFile (vm2asm path) out
--     _  -> putStrLn "Error - too many command line arguments."
--
-- vm2asm :: FilePath -> FilePath
-- vm2asm path = if isSuffixOf ".vm" path
--                 then reverse . ("msa" ++) . drop 2 $ reverse path
--                 else makeFilename path
--
-- dropvm :: FilePath -> Builder
-- dropvm = T.fromString . reverse . takeWhile (/= '/') . drop 3 . reverse
--
-- makeFilename :: FilePath -> String
-- makeFilename path = path ++
--                   ( reverse . ("msa." ++) . takeWhile (/= '/') . drop 1 . reverse
--                   $ path
--                   )
