module Imperative (
    Expr(..)
  , Program
  , translate  
  , compile
  ) where

import Text.Parsec

import qualified BoBeep as B
import qualified Lambda as L

-- Very minimal subset of any imperative language that supports function values
--   -only unary functions
--   -only return [expr] as function bodies

data Expr = 
    FnCall Expr Expr
  | FnDef (Maybe String) String Expr
  | Var String
  deriving (Show, Eq)

type Program = [Expr]

nameFunction :: String -> Expr -> Expr
nameFunction name (FnDef _ arg body) = FnDef (Just name) arg body
nameFunction name e = e

compile :: SourceName -> String -> Either ParseError Program
compile srcName src = fmap translate $ B.parseSrc srcName src

-- Translate a BoBeep program
translate :: B.Program -> Program
translate = fmap translateBoBeep

-- Translate a single statement or decl from BoBeep
translateBoBeep :: B.BoBeep -> Expr
translateBoBeep (B.Stmt term) = translateTerm term
translateBoBeep (B.Decl name term) = nameFunction name (translateTerm term)

-- Translate a single lambda term 
translateTerm :: L.Term' -> Expr
translateTerm (L.Var v) = Var $ v
translateTerm (L.Abs arg body) = FnDef Nothing arg (translateTerm body)
translateTerm (L.App t1 t2) = FnCall (translateTerm t1) (translateTerm t2)
