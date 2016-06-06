module Lambda where

import Control.Monad.Reader
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as T

type LambdaParser s a = Parsec String s a

type VarName = String

-- term = \var term | term term | var
data Term var =
    Abs var (Term var)
  | App (Term var) (Term var)
  | Var var 
  deriving (Show, Eq)

type Term' = Term VarName

-- XXX: shitty Ord constraint
findUnboundVarsTopLevel :: Ord var => Term var -> [var]
findUnboundVarsTopLevel term = runReader (findUnboundVars term) S.empty 

findUnboundVars :: Ord var => Term var -> Reader (S.Set var) [var]
findUnboundVars (Var v) = do
  boundVars <- ask
  if v `S.member` boundVars
    then return [] 
    else return [v]
findUnboundVars (Abs arg body) =
  local (S.insert arg) $ findUnboundVars body    
findUnboundVars (App t1 t2) =
  (++) <$> findUnboundVars t1 <*> findUnboundVars t2
  
  
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
