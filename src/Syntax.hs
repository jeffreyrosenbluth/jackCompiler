{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import           Data.Word
import           TextShow

data Statement
  = Let String (Maybe Expression) Expression
  | If Expression [Statement] (Maybe [Statement])
  | While Expression [Statement]
  | Do SubCall
  | Return (Maybe Expression)
  deriving Show

data Expression
  = ConstantE Constant
  | BinOpE BinOp Expression Expression
  | UnOpE UnOp Expression
  | VarE String (Maybe Expression)
  | CallE SubCall
  deriving Show

data Constant
  = IntC Word16
  | StringC String
  | KeywordC Keyword
  deriving Show

data SubCall = SubCall String [Expression]
  deriving Show

data ProcType = Constructor | Function | Method
  deriving (Eq, Show)

data Procedure = Procedure
  { procType :: ProcType
  , retType  :: Maybe Type
  , name     :: String
  , args     :: [(Type, String)]
  , vars     :: [Var]
  , stmts    :: [Statement]
  } deriving Show

data Keyword = Yes | No | Null | This
  deriving Show

data BinOp = Plus | Minus | Times | Divide | And | Or | Greater | Less | Equal
  deriving Show

instance TextShow BinOp where
  showb = \case
    Plus -> "add\n"
    Minus -> "sub\n"
    Times -> "call Math.multiply 2\n"
    Divide -> "call Math.divide 2\n"
    And -> "and\n"
    Or -> "or\n"
    Greater -> "gt\n"
    Less -> "lt\n"
    Equal -> "eq\n"

data UnOp = Neg | Not
  deriving Show

instance TextShow UnOp where
  showb = \case
    Neg -> "neg\n"
    Not -> "not\n"

data Type
  = IntT
  | CharT
  | BooleanT
  | ClassT String
  deriving Show

cName :: Type -> String
cName (ClassT s) = s
cName _          = error "Not a class name"

data ClassVar
  = Static Type [String]
  | Field Type [String]
  deriving Show

data Var = Var Type [String]
  deriving Show

data Class = Class String [ClassVar] [Procedure]
  deriving Show
