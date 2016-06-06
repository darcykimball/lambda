module Imperative where

-- Very minimal subset of any imperative language that supports function values
--   -only unary functions
--   -only return [expr] as function bodies

data Expr = 
    FnCall Expr Expr
  | FnDef String (Maybe String) Expr
  | Var String
  deriving (Show, Eq)

type Prog = [Expr]
