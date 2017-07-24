{-# LANGUAGE TemplateHaskell #-}

module Symbol where

import           Control.Lens (makeLenses)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Word

import           Syntax


data ClassKind = Stat Word16 | Fld Word16
  deriving Show

data SubKind   = Arg Word16  | Local Word16
  deriving Show

data Symbol a = Symbol
  { _typ  :: Type
  , _kind :: a
  } deriving Show
makeLenses ''Symbol

type SymbolTable a = Map String (Symbol a)

type ClassTable = Map String (Symbol ClassKind)
type SubTable   = Map String (Symbol SubKind)
