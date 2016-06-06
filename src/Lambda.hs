module Lambda where

import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Text.Parsec.Char
import Text.Parsec.String

type LambdaParser s a = Parsec String s a

type VarName = String

-- term = \var term | term term | var
data Term var =
    Abs var (Term var)
  | App (Term var) (Term var)
  | Var var 
  deriving (Show, Eq)

type Term' = Term VarName

findUnboundVars :: Eq var => Term var -> [var]
findUnboundVars = undefined

{-
 - Parsing
 -}

lexer = T.makeTokenParser lambdaDef
  where
    lambdaDef = emptyDef {
          T.identStart = letter
        , T.identLetter = letter
        , T.nestedComments = False
        , T.commentLine = "#"
        , T.caseSensitive = True
      }

parseTopLevel :: LambdaParser s (Term VarName)
parseTopLevel = parseTerm <* eof

parseNonApp :: LambdaParser s (Term VarName)
parseNonApp = parseVar <|> parseAbs <|> parens parseTerm

parseTerm :: LambdaParser s (Term VarName)
parseTerm = chainl1 parseNonApp (pure App)

parseAbs :: LambdaParser s (Term VarName)
parseAbs = Abs <$> (lambdaSymbol *> parseVarName) <*> parseTerm

parseVar :: LambdaParser s (Term VarName)
parseVar = Var <$> parseVarName

parseVarName :: LambdaParser s VarName
parseVarName = T.identifier lexer

lambdaSymbol :: LambdaParser s ()
lambdaSymbol = () <$ T.symbol lexer lambdaStr
  where
    lambdaStr = "\\"

parens :: LambdaParser s a -> LambdaParser s a
parens = T.parens lexer

parseWith :: String -> Either ParseError (Term VarName)
parseWith = runParser parseTopLevel [] "(Test in parseWith)"
