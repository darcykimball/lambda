import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Text
import qualified Data.Text as T
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

  src <- T.unpack <$> runResourceT (CB.sourceFile inputFile $$ decodeUtf8 $= textSink)

  either print print $ translate targetLang inputFile src
  where
    opts = info (helper <*> (liftA2 (,) file lang)) (fullDesc <> progDesc desc)
    desc = "Translate some lambda terms into JavaScript, Python, or Perl."
      
    textSink :: MonadResource m => Sink T.Text m T.Text
    textSink = do
      mtext <- await
      maybe (return T.empty) return mtext
  
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
