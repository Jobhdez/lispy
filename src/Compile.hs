module Compile where

import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser
import Desugar
import ToAnf
import ToCir
import ToSelect
import ToStack
import Tox86

compile :: String -> String
compile exp =
  tox86 (toStack (toselect (makeexplicit (toanf (parseExp (lexer exp))))))

writeToFile :: FilePath -> String -> IO ()
writeToFile  filePath content = do
  withFile filePath WriteMode $ \handle -> do
    TIO.hPutStr handle (T.pack content)

main :: IO ()
main = do
  let asm = compile "(let ((sum 0)) (let ((i 0)) (begin (while (< i 5) (begin (set sum (+ sum i)) (set i (+ i 1)))) sum)))" in
    writeToFile "example2.s" asm
