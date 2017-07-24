{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module CodeGen where

import           Symbol
import           Syntax

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Foldable
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Monoid
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy           as T
import           Data.Text.Lazy.Builder   (Builder)
import qualified Data.Text.Lazy.Builder   as T
import           Data.Word
import           TextShow

data Model = Model
  { _classTable  :: ClassTable
  , _subTable    :: SubTable
  , _staticCount :: Word16
  , _fieldCount  :: Word16
  , _argCount    :: Word16
  , _localCount  :: Word16
  } deriving Show
makeLenses ''Model

genClass :: MonadState Model m => Class -> m ()
genClass (Class s cvs sds) = do
  traverse_ classVar cvs
  traverse_ subDecl  sds

addSymbol :: MonadState Model m
          => Lens' Model (SymbolTable a)
          -> (Word16 -> a)
          -> Lens' Model Word16
          -> Type
          -> String
          -> m ()
addSymbol table kind count ty name = do
  c <- use count
  table %= M.insert name (Symbol ty (kind c))
  count += 1

addSymbol' :: MonadState Model m
          => Lens' Model (SymbolTable a)
          -> (Word16 -> a)
          -> Lens' Model Word16
          -> (Type, String)
          -> m ()
addSymbol' table kind count (ty, name) =
  addSymbol table kind count ty name

classVar :: MonadState Model m => ClassVar -> m ()
classVar cv = do
  case cv of
    Static ty vars ->
      traverse_ (addSymbol classTable Stat staticCount ty) vars
    Field ty vars  ->
      traverse_ (addSymbol classTable Fld fieldCount ty) vars

subDecl :: MonadState Model m => SubDecl -> m ()
subDecl sd = do
  case sd of
    Constructor _ _ xs ys _ -> addSyms xs ys
    Function    _ _ xs ys _ -> addSyms xs ys
    Method      _ n xs ys _ -> addSyms ((ClassT n, "this"):xs) ys
  where
    addSyms args vs = do
      traverse_ (addSymbol' subTable Arg argCount ) args
      traverse_ (\(Var ty ws) ->
        traverse_ (addSymbol subTable Local localCount ty) ws) vs

