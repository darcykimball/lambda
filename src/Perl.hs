module Perl where

import Text.Parsec

import qualified Imperative as I
import qualified Lambda as L

compile :: I.Program -> String
compile = undefined

compileExpr :: I.Expr -> String
compileExpr (I.FnCall e1 e2) =
  wrapParens (compileExpr e1) ++ wrapParens (compileExpr e2)
compileExpr (I.Var v) = wrapSpace v
compileExpr (I.FnDef mname arg body) =
  maybe "" (++ " = ") mname ++ "function" ++
    (wrapSpace . wrapParens) arg ++
    block ("return " ++ compileExpr body ++ ";")

-- Utility fns

block = ("{ " ++) . (++ " }")
wrapParens = ('(':) . (++ ")")
wrapSpace = (' ':) . (++ " ")
