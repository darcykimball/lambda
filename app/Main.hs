import System.IO

import JavaScript
import qualified Combinator as C

main :: IO ()
main = do
  hPutStrLn stderr "Reading one line expression from stdin..."
  input <- translateSKI combNames <$> getLine
  putStrLn $ unlines (compile' <$> dumpSKI combNames) 
  putChar '\n'
  putStrLn $ either show compile' input
  where
    combNames = ("S", "K", "I")
