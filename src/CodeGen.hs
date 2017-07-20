module CodeGen where

import           Syntax
import           Symbol

import           Control.Monad.State.Lazy
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Word
import           TextShow

data Model =
  { classTable :: ClassTable
  , classVar   :: Word16
  , subTable   :: SubTable
  , subVar     :: Word16
  }

incClassVar  :: State Model ()
incClassVar = modify (\model -> model {classVar = classVar model + 1})

incSubVar  :: State Model ()
incSubVar = modify (\model -> model {subVar = subVar model + 1})