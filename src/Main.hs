module Main where

import           Syntax
import           Lexer
import           Parser

import           Data.List             (isSuffixOf)
import qualified Data.Text.Lazy.IO     as T
import           System.Environment    (getArgs)
import           System.FilePath.Find
import           Text.Megaparsec

main ::IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      files <- find (depth <? 10) (extension ==? ".jack") path
      outB  <- traverse parseFile files
      print outB



parseFile :: FilePath -> IO (Either (ParseError Char Dec) Class)
parseFile fp = do
  f <- T.readFile fp
  return $ parse (contents parseClass) "<stdin>" f

vm2asm :: FilePath -> FilePath
vm2asm path = if isSuffixOf ".vm" path
                then reverse . ("msa" ++) . drop 2 $ reverse path
                else makeFilename path

makeFilename :: FilePath -> String
makeFilename path = path ++
                  ( reverse . ("msa." ++) . takeWhile (/= '/') . drop 1 . reverse
                  $ path
                  )
