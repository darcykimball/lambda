module JavaScript where

import Text.Parsec

import qualified Lambda as L

-- Very minimal subset of javascript
--   -only unary functions
--   -only return [expr] as function bodies

data Expr = 
    FnCall Expr Expr
  | FnDef String (Maybe String) Expr
  | Var String
  deriving (Show, Eq)

type Prog = [Expr]

translate :: String -> Either ParseError Expr
translate = fmap translate' . runParser L.parseTopLevel [] "(Translating to JS)"

translate' :: L.Term L.VarName -> Expr
translate' (L.Var v) = Var $ v
translate' (L.Abs arg body) = FnDef arg Nothing (translate' body)
translate' (L.App t1 t2) = FnCall (translate' t1) (translate' t2)

compile :: String -> Either ParseError String
compile = fmap compile' . translate

compile' :: Expr -> String
compile' (FnCall e1 e2) = wrapParens (compile' e1) ++ wrapParens (compile' e2)
compile' (Var v)        = wrapSpace v
compile' (FnDef arg mname body) =
  maybe "" (++ " = ") mname ++ "function" ++
    (wrapSpace . wrapParens) arg ++ block ("return " ++ compile' body ++ ";")

-- Utility fns

block = ("{ " ++) . (++ " }")
wrapParens = ('(':) . (++ ")")
wrapSpace = (' ':) . (++ " ")

