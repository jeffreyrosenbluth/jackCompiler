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
  { _className   :: String
  , _classTable  :: SymbolTable
  , _subTable    :: SymbolTable
  , _staticCount :: Word16
  , _fieldCount  :: Word16
  , _argCount    :: Word16
  , _localCount  :: Word16
  } deriving Show
makeLenses ''Model

genClass :: MonadState Model m => Class -> m ()
genClass (Class nm cvs sds) = do
  className .= nm
  traverse_ classVar cvs
  traverse_ procedure  sds

addSymbol :: MonadState Model m
          => Lens' Model SymbolTable
          -> SKind
          -> Lens' Model Word16
          -> Type
          -> String
          -> m ()
addSymbol table knd count ty nm = do
  c <- use count
  table %= M.insert nm (Symbol ty knd c)
  count += 1

addSymbol' :: MonadState Model m
          => Lens' Model SymbolTable
          -> SKind
          -> Lens' Model Word16
          -> (Type, String)
          -> m ()
addSymbol' table knd count (ty, nm) =
  addSymbol table knd count ty nm

classVar :: MonadState Model m => ClassVar -> m ()
classVar = do
  \case
    Static ty vs ->
      traverse_ (addSymbol classTable Stat staticCount ty) vs
    Field ty vs  ->
      traverse_ (addSymbol classTable Fld fieldCount ty) vs

procedure :: MonadState Model m => Procedure -> m Builder
procedure (Procedure pType rType nm xs ys zs) = do
  addSyms xs' ys
  cn <- use className
  n  <- use localCount
  pure $ "function " <> showb cn <> "." <> showb nm <> " " <> showb n <> "\n"
  where
    xs' = if pType == Method then (ClassT nm, "this"):xs else xs
    addSyms as vs = do
      traverse_ (addSymbol' subTable Arg argCount ) as
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
  CallE sc       -> subCall sc

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
segmentOf Fld   = THIS
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

subCall :: MonadState Model m => SubCall -> m Builder
subCall (SubCall s es) = undefined
