import Options.Applicative
import System.IO

import JavaScript

main :: IO ()
main = execParser opts >>= print
  where
    opts = info (helper <*> lang) (fullDesc <> progDesc desc)
    desc = "Translate some lambda terms into JavaScript, Python, or Perl."
  
data Lang = JavaScript | Python | Perl deriving (Eq, Show, Read)

lang :: Parser Lang
lang = option auto
         (long "lang" 
          <> short 'l' 
          <> metavar "LANGUAGE" 
          <> value JavaScript
          <> help "Target language")
