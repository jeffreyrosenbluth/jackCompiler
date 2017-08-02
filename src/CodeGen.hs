{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module CodeGen where

import           Symbol
import           Syntax

import           Control.Lens             hiding (index)
import           Control.Monad.State.Lazy
import           Data.Foldable
-- import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Monoid
-- import           Data.Text.Lazy           (Text)
-- import qualified Data.Text.Lazy           as T
import           Data.Text.Lazy.Builder   (Builder)
-- import qualified Data.Text.Lazy.Builder   as T
import           Data.Word
import           TextShow

data Model = Model
  { _classTable  :: SymbolTable
  , _subTable    :: SymbolTable
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
          => Lens' Model SymbolTable
          -> SKind
          -> Lens' Model Word16
          -> Type
          -> String
          -> m ()
addSymbol table knd count ty name = do
  c <- use count
  table %= M.insert name (Symbol ty knd c)
  count += 1

addSymbol' :: MonadState Model m
          => Lens' Model SymbolTable
          -> SKind
          -> Lens' Model Word16
          -> (Type, String)
          -> m ()
addSymbol' table knd count (ty, name) =
  addSymbol table knd count ty name

classVar :: MonadState Model m => ClassVar -> m ()
classVar = do
  \case
    Static ty vars ->
      traverse_ (addSymbol classTable Stat staticCount ty) vars
    Field ty vars  ->
      traverse_ (addSymbol classTable Fld fieldCount ty) vars

subDecl :: MonadState Model m => SubDecl -> m ()
subDecl = do
  \case
    Constructor _ _ xs ys _ -> addSyms xs ys
    Function    _ _ xs ys _ -> addSyms xs ys
    Method      _ n xs ys _ -> addSyms ((ClassT n, "this"):xs) ys
  where
    addSyms args vs = do
      traverse_ (addSymbol' subTable Arg argCount ) args
      traverse_ (\(Var ty ws) ->
        traverse_ (addSymbol subTable Local localCount ty) ws) vs

expression :: MonadState Model m => Expression -> m Builder
expression = \case
  ConstantE c    -> constant c
  BinOpE b e1 e2 -> binop b e1 e2
  UnOpE u e      -> unop u e
  VarE s _       -> do
    ct <- gets _classTable
    st <- gets _subTable
    case symbol s [st, ct] of
                      Nothing -> error "Variable not defined"
                      Just v  -> pure $ push (segmentOf . _sKind $ v)
                                             (_index v)

data Segment = ARG | LCL | STC | CONST | THIS | THAT | PNTR | TMP

instance TextShow Segment where
  showb = \case
    ARG   -> "argument "
    LCL   -> "local "
    STC   -> "static "
    CONST -> "constant "
    THIS  -> "this "
    THAT  -> "that "
    PNTR  -> "pointer "
    TMP   -> "temp "

segmentOf :: SKind -> Segment
segmentOf Stat  = STC
segmentOf Fld   = THIS -- XXX This is almost surely wrong.
segmentOf Arg   = ARG
segmentOf Local = LCL

push :: Segment -> Word16 -> Builder
push s n = "push " <> showb s <> showb n <> "\n"

constant :: MonadState Model m => Constant -> m Builder
constant = \case
  IntC n        -> pure $ p n
  StringC s     -> undefined
  KeywordC Yes  -> pure $ p 1 <> "neg\n"
  KeywordC This -> undefined
  KeywordC _    -> pure $ p 0
  where
    p x = push CONST x <> "\n"

binop :: MonadState Model m => BinOp -> Expression -> Expression -> m Builder
binop b e1 e2 = do
  x <- expression e1
  y <- expression e2
  pure $ x <> y <> showb b

unop :: MonadState Model m => UnOp -> Expression -> m Builder
unop u e = mappend <$> expression e <*> pure (showb u)
