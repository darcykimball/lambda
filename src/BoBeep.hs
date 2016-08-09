module BoBeep where

import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.Token

import qualified Lambda as L

{-
 - Types
 -}

-- Type of term synonym names
type SynName = String

-- Statements, e.g.:
--   (\x y x) (\x z)
--   (\x x) S
--
-- Declarations, e.g.:
--   S := \x \y \z x z (y z)
--   I := \x x
--
-- All variables must be bound(?).
data BoBeep =   
    Stmt L.Term'         -- A statement; 'evaluate' an expression
  | Decl SynName L.Term' -- A declaration of a lambda term synonym
  deriving (Show)

type Program = [BoBeep]

type BoundNames = S.Set SynName
    
{-
 - Parsing
 -}

type BoBeepParser a = Parsec String BoundNames a

parseSrc :: SourceName -> String -> Either ParseError Program
parseSrc = runParser (parseProgram <* spaces <* eof) S.empty

parseProgram :: BoBeepParser Program
parseProgram = many1 parseBoBeep

parseBoBeep :: BoBeepParser BoBeep
parseBoBeep = (try parseStmt <|> parseDecl) <* endSymbol

parseDecl :: BoBeepParser BoBeep
parseDecl = do
  name <- symName
  bind
  bindings <- getState
  term <- L.parseTerm
  if name `S.member` bindings
    then fail $ "Name " ++ name ++ " already bound!"
    -- FIXME: need to check that body of fn only contains abstracted vars and
    -- synonym names (bound already)
    else modifyState (S.insert name) >> return (Decl name term)
  where
    bind = symbol L.lexer ":="
    symName = identifier L.lexer

parseStmt :: BoBeepParser BoBeep
parseStmt = do
  term <- L.parseTerm
  bindings <- getState
  let unknownVars =
        filter (not . flip S.member bindings) $ L.findUnboundVarsTopLevel term
  if null unknownVars
    then return $ Stmt term
    else fail $ unlines $ fmap nameNotFoundMsg unknownVars
  where
    nameNotFoundMsg name =
      "Name " ++ name ++ " is either unbound or undeclared"

endSymbol :: BoBeepParser ()
endSymbol = () <$ semi L.lexer 
