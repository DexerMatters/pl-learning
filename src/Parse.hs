module Parse (Parser, pTm) where

import Data.Void (Void)
import Terms (Raw (..))
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, many, some, (<|>))
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))

type Parser = Parsec Void String

type ParseStr = Parser String

pres :: [String]
pres = ["let", "if", "then", "else", "Top", "Bot"]

d :: (Show a) => Parser a -> Parser a
d = dbg "debug"

-- Basic Parsers
ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: ParseStr -> ParseStr
lexeme = L.lexeme ws

symbol :: String -> ParseStr
symbol = L.symbol ws

var :: ParseStr
var = lexeme $ do
  str <- some C.letterChar
  if str `elem` pres
    then fail $ "keyword " ++ str ++ " is reserved"
    else return str

paren :: Parser a -> Parser a
paren p = symbol "(" *> p <* symbol ")"

bracket :: Parser a -> Parser a
bracket p = symbol "[" *> p <* symbol "]"

brace :: Parser a -> Parser a
brace p = symbol "{" *> p <* symbol "}"

-- AST Parsers

pVar, pLam, pLet, pApp, pPath, pPi, pTypeof :: Parser Raw
pVar = RVar <$> var
pLam =
  RLam
    <$> (symbol "\\" *> var)
    <*> (symbol "->" *> pTm 1)
pLet =
  RLet
    <$> (symbol "let" *> var) -- bound name
    <*> (symbol "::" *> pTm' 0) -- type
    <*> (symbol "=" *> pTm 1) -- value
    <*> (symbol ";" *> pTm 0) -- scope
pApp = RApp <$> (pTm 3 <* ws) <*> pTm 2
pPath = brace $ RPath <$> pTm' 2 <*> (symbol "..." *> pTm' 2)
pPi =
  RPi
    <$> var
    <*> (symbol ":" *> pTm' 2)
    <*> (symbol "->" *> pTm' 0)
pTypeof = RTyOf <$> (symbol "typeof" *> pTm 3)

pTm :: Int -> Parser Raw
pTm i =
  choice $
    paren (pTm 0 <|> pTm' 0)
      : drop
        i
        [ pLet,
          try pLam,
          try pApp,
          try pVar
        ]

pTm' :: Int -> Parser Raw
pTm' i =
  choice $
    paren (pTm' 0 <|> pTm 0)
      : drop
        i
        [ try pPi,
          try pPath,
          pTypeof,
          RUni <$ symbol "{U}",
          RTop <$ (symbol "Top" <|> symbol "⊤"),
          RBot <$ (symbol "Bot" <|> symbol "⊥"),
          try pVar
        ]