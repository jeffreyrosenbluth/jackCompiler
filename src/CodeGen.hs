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
import           Data.Char                (ord)
import           Data.Foldable
import           Data.List                (intersperse)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Text.Lazy.Builder   (Builder)
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
addSymbol' table knd count (ty, nm) = addSymbol table knd count ty nm

classVar :: MonadState Model m => ClassVar -> m ()
classVar = \case
  Static ty vs -> traverse_ (addSymbol classTable Stat staticCount ty) vs
  Field ty vs  -> traverse_ (addSymbol classTable Fld fieldCount ty) vs

statement :: MonadState Model m => Statement -> m Builder
statement (Let s mExpr expr) = do
  e <- expression expr
  ts <- gets tables
  let sym = fromMaybe (error "Variable not in scope") (symbol s ts)
  case mExpr of
    Nothing -> do
      let x = pop (segmentOf (sym ^. sKind)) (sym ^. index)
      pure $ e <> x
    Just mE -> do
      idx <- expression mE
      let x = push (segmentOf (sym ^. sKind)) (sym ^. index)
      pure $ mconcat
        [idx, x, "add\n", e, pop TMP 0, pop PNTR 1, push TMP 0, pop THAT 0]

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
  pure $ mconcat
    [ne, "if-goto ", l1, ss', "goto ", l2, "label ", l1, mss', "label ", l2]

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
  pure $ mconcat
    ["label ", l1, ne, "if-goto ", l2, ss',"goto ", l1, "label ", l2]

statement (Do sc) = do
  s <- subCall sc
  pure $ s <> pop TMP 0

statement (Return mExpr) = case mExpr of
  Nothing -> pure $ push CONST 0 <> "return\n"
  Just expr -> do
    e <- expression expr
    pure $ e <> "return\n"

constructor :: Word16 -> Builder
constructor n
  | n == 0 = pop PNTR 0
  | otherwise = push CONST n <> "call Memory.alloc 1\n" <> pop PNTR 0

method :: Builder
method = push ARG 0 <> pop PNTR 0

procedure :: MonadState Model m => Procedure -> m Builder
procedure (Procedure pType _ nm xs ys zs) = do
  argCount   .= 0
  localCount .= 0
  addSyms xs' ys
  cn <- use className
  n  <- use localCount
  fn <- use fieldCount
  ss <- mconcat <$> traverse statement zs
  let specific = case pType of
        Constructor -> constructor fn
        Method      -> method
        Function    -> ""
  pure $ mconcat
    [ "function "
    , fromString cn, "."
    , fromString nm, " "
    , showb n, "\n"
    , specific, ss
    ]
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
  VarE s e       -> do
    tbls <- gets tables
    case e of
      Nothing -> do
        case symbol s tbls of
          Nothing -> error "Variable not defined"
          Just v  -> pure $ push (segmentOf $ v ^. sKind) (v ^. index)
      Just e' -> do
        case symbol s tbls of
          Nothing -> error "Variable not defined"
          Just v  -> do
            idx <- expression e'
            let l1 = push (segmentOf $ v ^. sKind) (v ^. index)
                l2 = "add\n"
                l3 = pop PNTR 1
                l4 = push THAT 0
            pure $ mconcat [idx, l1, l2, l3, l4]

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
  StringC s     -> do
    let l1 = push CONST (fromIntegral . length $ s)
        l2 = "call String.new 1\n"
        l3 = push CONST . fromIntegral . ord <$> s
        l4 = "call String.appendChar 2\n"
    pure $ mconcat [l1, l2, mconcat (intersperse l4 l3), l4]
  KeywordC Yes  -> pure $ p 0 <> "not\n"
  KeywordC This -> pure $ push PNTR 0
  KeywordC _    -> pure $ p 0
  where
    p x = push CONST x

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
  cn <- use className
  m  <- get
  case symbol (takeWhile (/= '.') s) (tables m) of
    Just v -> do
      let nm = drop 1 . dropWhile (/= '.') $ s
          s' = cName (v ^. sType) <> "." <> nm
          sg = segmentOf (v ^. sKind)
          i  = v ^. index
          o  = push sg i
      pure $ o <> es <> "call " <> fromString s' <> " " <> showb (1 + length exprs) <> "\n"
    Nothing ->
      if '.' `elem` s
        then pure $ es <> "call "
                       <> fromString s
                       <> " "
                       <> showb (length exprs)
                       <> "\n"
        else pure $ push PNTR 0
                 <> es
                 <> "call "
                 <> fromString (cn <> "." <> s)
                 <> " "
                 <> showb (1 + length exprs)
                 <> "\n"
