module Main where

import           CodeGen
import           Lexer
import           Parser
import           Syntax

import           Control.Monad.State.Lazy (evalState, zipWithM_)
import           Data.Either              (rights)
import           Data.List                (isSuffixOf)
import           Data.Map                 (empty)
import qualified Data.Text.Lazy.IO        as T
import           System.Environment       (getArgs)
import           System.FilePath.Find     (depth, extension, find, (<?), (==?))
import           Text.Megaparsec          (Dec, ParseError, parse)
import           TextShow                 (toLazyText)

main ::IO ()
main = do
  paths <- getArgs
  case paths of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      files <- find (depth <? 10) (extension ==? ".jack") path
      outB  <- traverse parseFile files
      let results = rights outB
          fnames  = jack2vm <$> files
          cBldr   = evalState (traverse genClass results)
                              (Model "" empty empty 0 0 0 0 0)
      zipWithM_ T.writeFile fnames (toLazyText <$> cBldr)

parseFile :: FilePath -> IO (Either (ParseError Char Dec) Class)
parseFile fp = do
  f <- T.readFile fp
  pure $ parse (contents parseClass) "<stdin>" f

jack2vm :: FilePath -> FilePath
jack2vm path
  | isSuffixOf ".jack" path = reverse . ("mv" ++) . drop 4 $ reverse path
  | otherwise               = makeFilename path

makeFilename :: FilePath -> String
makeFilename path =
  path ++ ( reverse . ("mv." ++) . takeWhile (/= '/') . drop 1 . reverse $ path)
