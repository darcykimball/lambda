module BoBeep where

import Text.Parsec
import Text.Parsec.Token
import qualified Data.Set as S

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

parseProgram :: BoBeepParser Program
parseProgram = many1 parseBoBeep

parseBoBeep :: BoBeepParser BoBeep
parseBoBeep = parseDecl <|> parseStmt

parseDecl :: BoBeepParser BoBeep
parseDecl = do
  name <- symName
  bind
  bindings <- getState
  term <- L.parseTerm
  if name `S.member` bindings
    then fail $ "Name " ++ name ++ " already bound!"
    else modifyState (S.insert name) >> return (Decl name term)
  where
    bind = symbol L.lexer ":="
    symName = identifier L.lexer

parseStmt :: BoBeepParser BoBeep
parseStmt = do
  term <- L.parseTerm
  bindings <- getState
  let unknownVars =
        filter (not . flip S.member bindings) $ L.findUnboundVars term
  if null unknownVars
    then return $ Stmt term
    else fail $ unlines $ fmap nameNotFoundMsg unknownVars
  where
    nameNotFoundMsg name =
      "Name " ++ name ++ " is either unbound or undeclared"
