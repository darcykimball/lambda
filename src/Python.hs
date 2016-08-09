module Python where

import Text.Parsec

import qualified Imperative as I
import qualified Lambda as L

compile :: I.Program -> String
compile = unlines . fmap compileExpr

compileExpr :: I.Expr -> String
compileExpr (I.FnCall e1 e2) =
  wrapParens (compileExpr e1) ++ wrapParens (compileExpr e2)
compileExpr (I.Var v) = wrapSpace v
compileExpr (I.FnDef mname arg body) =
  case mname of
    Nothing -> -- Lambda
      "lambda " ++ arg ++ ": " ++ compileExpr body
    Just name -> -- Top-level
      "def " ++ name ++ wrapParens arg ++ ": return " ++ compileExpr body

block = ("{ " ++) . (++ " }")
wrapParens = ('(':) . (++ ")")
wrapSpace = (' ':) . (++ " ")
