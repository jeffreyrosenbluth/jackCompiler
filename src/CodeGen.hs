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
import           Data.Maybe               (fromMaybe)
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
  , _labelCount  :: Word16
  } deriving Show
makeLenses ''Model

tables :: Model -> [SymbolTable]
tables m = [m ^. subTable, m ^. classTable]

genClass :: MonadState Model m => Class -> m Builder
genClass (Class nm cvs sds) = do
  className .= nm
  traverse_ classVar cvs
  mconcat <$> traverse procedure sds

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

statement :: MonadState Model m => Statement -> m Builder
statement (Let s mexpr expr) = do
  e <- expression expr
  ts <- gets tables
  let sym = symbol s ts
      x   = case sym of
        Nothing   -> error "Variable not in scope"
        Just sym' -> pop (segmentOf (sym' ^. sKind)) (sym' ^. index)
  pure $ e <> x

statement (If expr ss mss) = do
  e <- expression expr
  ss' <- mconcat <$> traverse statement ss
  l1Num <- use labelCount
  labelCount += 1
  l2Num <- use labelCount
  labelCount += 1
  let ne = e <> "not\n"
      el = fromMaybe [] mss
      l1  = "L" <> showb l1Num <> "\n"
      l2  = "L" <> showb l2Num <> "\n"
  mss' <- mconcat <$> traverse statement el
  pure $ ne <> "if-goto "
            <> l1
            <> ss'
            <> "goto "
            <> l2
            <> "label "
            <> l1
            <> mss'
            <> "label "
            <> l2

statement (While expr ss) = do
  e <- expression expr
  ss' <- mconcat <$> traverse statement ss
  l1Num <- use labelCount
  labelCount += 1
  l2Num <- use labelCount
  labelCount += 1
  let ne = e <> "not\n"
      l1  = "L" <> showb l1Num <> "\n"
      l2  = "L" <> showb l2Num <> "\n"
  pure $ "label " <> l1
                  <> ne
                  <> "if-goto " <> l2
                  <> ss'
                  <> "goto " <> l1
                  <> "label " <> l2


statement (Do sc) = subCall sc

statement (Return mExpr) = case mExpr of
  Nothing -> pure $ pop TMP 0 <> "return\n"
  Just expr -> do
    e <- expression expr
    pure $ e <> "return\n"

procedure :: MonadState Model m => Procedure -> m Builder
procedure (Procedure pType rType nm xs ys zs) = do
  argCount .= 0
  localCount .= 0
  addSyms xs' ys
  cn <- use className
  n  <- use localCount
  ss <- mconcat <$> traverse statement zs
  let meth = if pType == Method then push ARG 0 <> pop PNTR 0 else ""
  pure $ "function " <> fromString cn
                     <> "."
                     <> fromString nm
                     <> " "
                     <> showb n
                     <> "\n"
                     <> meth
                     <> ss
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
    ct <- use classTable
    st <- use subTable
    case symbol s [st, ct] of
                      Nothing -> error "Variable not defined"
                      Just v  -> pure $ push (segmentOf $ v ^. sKind)
                                             (v ^. index)
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

pop :: Segment -> Word16 -> Builder
pop s n = "pop " <> showb  s <> showb n <> "\n"

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
subCall (SubCall s exprs) = do
  es <- mconcat <$> traverse expression exprs
  pure $ es <> "call " <> fromString s <> " " <> showb (length exprs) <> "\n"
