module Combinator where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token as T
import Text.Parsec.String

data SKI =
    S
  | I
  | K
  | App SKI SKI
  deriving (Show)

type SKIParser a = Parser a

lexer = T.makeTokenParser skiDef
  where
    skiDef = emptyDef {
          T.identStart = letter
        , T.identLetter = letter
        , T.nestedComments = False
        , T.commentLine = "#"
        , T.caseSensitive = True
      }

parseWith :: String -> Either ParseError SKI
parseWith = parse parseTopLevel "(Testing SKI)" 

parseTopLevel :: Parser SKI
parseTopLevel = parseSKI <* eof

parseS :: Parser SKI
parseS = S <$ T.symbol lexer "S"

parseI :: Parser SKI
parseI = I <$ T.symbol lexer "I"

parseK :: Parser SKI
parseK = K <$ T.symbol lexer "K"

parseNonApp :: Parser SKI
parseNonApp = parseS <|> parseK <|> parseI <|> Combinator.parens parseSKI

parseSKI :: Parser SKI
parseSKI = chainl1 parseNonApp (pure App)

parens = T.parens lexer
