module Main (main) where

import Lib
import Text.Megaparsec (parseTest, parse)
import Parser (pTm)
import Elab (check, emptyCtx, infer)

path :: String
path = "/home/dexer/Projects/Haskell/Projects/pl-learning/test/test.txt"

main :: IO ()
main = do
  str <- readFile path
  let raw = parse (pTm 0) "" str
  case raw of
    Left err -> print err
    Right ast -> print $ infer emptyCtx ast