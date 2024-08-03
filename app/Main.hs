module Main (main) where

import Lib
import Text.Megaparsec (parseTest, parse)
import Parse (pTm)
import Elab (infer, Ctx (Ctx))

path :: String
path = "/home/dexer/Projects/Haskell/Projects/pl-learning/test/test.txt"

main :: IO ()
main = do
  str <- readFile path
  parseTest (pTm 0) str
  case parse (pTm 0) "" str of
    Left err -> print err
    Right ast -> print $ do
      (_, ty) <- infer (Ctx [] [] 0) ast
      return ty
