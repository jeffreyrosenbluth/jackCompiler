module Syntax where

import Data.Word

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

data SubDecl
  = Constructor (Maybe Type) String [(Type, String)] [Var] [Statement]
  | Function (Maybe Type) String [(Type, String)] [Var] [Statement]
  | Method (Maybe Type) String [(Type, String)] [Var] [Statement]
  deriving Show

data Keyword = Yes | No | Null | This
  deriving Show

data BinOp = Plus | Minus | Times | Divide | And | Or | Greater | Less | Equal
  deriving Show

data UnOp = Neg | Not
  deriving Show

data Type
  = IntT
  | CharT
  | BooleanT
  | ClassT String
  deriving Show

data ClassVar
  = Static Type [String]
  | Field Type [String]
  deriving Show

data Var = Var Type [String]
  deriving Show

data Class = Class String [ClassVar] [SubDecl]
  deriving Show
