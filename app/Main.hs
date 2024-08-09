module Main (main) where

import Elab
import Eval (nf, quote)
import Lib
import Parse
import Text.Megaparsec (parse, parseTest)

path :: String
path = "/home/dexer/Projects/Haskell/Projects/pl-learning/test/test.txt"

main :: IO ()
main = do
  str <- readFile path
  parseTest (pTm 0) str
  case parse (pTm 0) path str of
    Left e -> print e
    Right tm -> case infer emptyCtx tm of
      Left e -> print e
      Right (_, ty) -> print $ (nf . quote) ty
