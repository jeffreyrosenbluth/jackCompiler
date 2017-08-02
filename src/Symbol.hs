{-# LANGUAGE TemplateHaskell #-}

module Symbol where

import           Control.Lens (makeLenses)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Maybe
import           Data.Word

import           Syntax

data SKind = Stat | Fld | Arg | Local
  deriving Show

data Symbol = Symbol
  { _sType :: Type
  , _sKind :: SKind
  , _index :: Word16
  } deriving Show
makeLenses ''Symbol

type SymbolTable = Map String Symbol

symbol :: String -> [SymbolTable] -> Maybe Symbol
symbol s xs = listToMaybe . catMaybes $ map (M.lookup s) xs

