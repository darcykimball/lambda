import Options.Applicative
import qualified Text.Parsec as Parsec
import System.IO

import qualified BoBeep as B
import qualified Imperative as I
import qualified JavaScript as J
import qualified Python as Py
import qualified Perl as P

main :: IO ()
main = do
  (inputFile, targetLang) <- execParser opts 
  src <- readFile inputFile
  either print print $ translate targetLang inputFile src
  where
    opts = info (helper <*> (liftA2 (,) file lang)) (fullDesc <> progDesc desc)
    desc = "Translate some lambda terms into JavaScript, Python, or Perl."
  
data Lang = JavaScript | Python | Perl deriving (Eq, Show, Read)

file :: Parser Parsec.SourceName
file = strOption (long "file" 
                  <> short 'f' 
                  <> metavar "FILE" 
                  <> help "Input file")

lang :: Parser Lang
lang = option auto
         (long "lang" 
          <> short 'l' 
          <> metavar "LANGUAGE" 
          <> value JavaScript
          <> help "Target language")

translate :: Lang -> Parsec.SourceName -> String -> Either Parsec.ParseError String
translate targetLang srcName src =
  fmap (compiler targetLang) $ I.compile srcName src
  where
    compiler JavaScript = J.compile
    compiler Python = Py.compile
    compiler Perl = P.compile
