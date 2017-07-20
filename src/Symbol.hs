module Symbol where

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Word

import           Syntax


data ClassKind = Static | Field

data SubKind = Arg | Local

data Symbol a = Symbol
  { typ :: Ty
  , kind :: a
  , num :: Word16
  }

type ClassTable = Map String (Symbol ClassKind)
type SubTable   = Map String (Symbol SubKind)