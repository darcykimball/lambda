module JavaScript where

import Text.Parsec

import qualified Imperative as I
import qualified Lambda as L

translate :: String -> Either ParseError I.Expr
translate = fmap translate' . runParser L.parseTopLevel [] "(Translating to JS)"

translate' :: L.Term L.VarName -> I.Expr
translate' (L.Var v) = I.Var $ v
translate' (L.Abs arg body) = I.FnDef arg Nothing (translate' body)
translate' (L.App t1 t2) = I.FnCall (translate' t1) (translate' t2)

compile :: String -> Either ParseError String
compile = fmap compile' . translate

compile' :: I.Expr -> String
compile' (I.FnCall e1 e2) =
  wrapParens (compile' e1) ++ wrapParens (compile' e2)
compile' (I.Var v) = wrapSpace v
compile' (I.FnDef arg mname body) =
  maybe "" (++ " = ") mname ++ "function" ++
    (wrapSpace . wrapParens) arg ++ block ("return " ++ compile' body ++ ";")

-- Utility fns

block = ("{ " ++) . (++ " }")
wrapParens = ('(':) . (++ ")")
wrapSpace = (' ':) . (++ " ")

