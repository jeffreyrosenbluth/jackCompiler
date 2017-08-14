module Main where

import           CodeGen
import           Lexer
import           Parser
import           Syntax

import           Control.Monad.State.Lazy
import           Data.Either
import           Data.List                (isSuffixOf)
import qualified Data.Map                 as M
import qualified Data.Text.Lazy.IO        as T
import           System.Environment       (getArgs)
import           System.FilePath.Find
import           Text.Megaparsec
import           TextShow

main ::IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStrLn "Error - no source file specified."
    [path] -> do
      files <- find (depth <? 10) (extension ==? ".jack") path
      outB  <- traverse parseFile files
      let results = rights outB
          fnames = jack2vm <$> files
          (a, b) = runState (traverse genClass results)
                        (Model "" M.empty M.empty 0 0 0 0 0)
      zipWithM_ T.writeFile fnames (TextShow.toLazyText <$> a)

parseFile :: FilePath -> IO (Either (ParseError Char Dec) Class)
parseFile fp = do
  f <- T.readFile fp
  return $ parse (contents parseClass) "<stdin>" f

jack2vm :: FilePath -> FilePath
jack2vm path = if isSuffixOf ".jack" path
                then reverse . ("mv" ++) . drop 4 $ reverse path
                else makeFilename path

makeFilename :: FilePath -> String
makeFilename path = path ++
                  ( reverse . ("mv." ++) . takeWhile (/= '/') . drop 1 . reverse
                  $ path
                  )
