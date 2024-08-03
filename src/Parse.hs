module Parse(Parser, pTm) where
import Text.Megaparsec (Parsec, many, (<|>), some, between, choice, MonadParsec (try))
import Data.Void (Void)
import Raw (Tm (..))
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Debug (MonadParsecDbg(dbg))


type Parser = Parsec Void String

type ParseStr = Parser String

pres :: [String]
pres = ["let", "if", "then", "else"]

d :: Show a => Parser a -> Parser a
d = dbg "debug"

-- Basic Parsers
ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: ParseStr -> ParseStr
lexeme = L.lexeme ws

symbol :: String -> ParseStr
symbol = L.symbol ws

var :: ParseStr
var = lexeme $ some C.letterChar

paren :: Parser a -> Parser a
paren p = symbol "(" *> p <* symbol ")"

bracket :: Parser a -> Parser a
bracket p = symbol "[" *> p <* symbol "]"

brace :: Parser a -> Parser a
brace p =symbol "{" *> p <* symbol "}"

-- AST Parsers

pVar, pCast, pLam, pLet, pApp, pCons, pConstPi, pPi, pTypeof, pDef :: Parser Tm
pVar = Var <$> var

pCast = Cast <$> bracket (pTm' 0) <*> pTm 5

pLam = Lam 
  <$> (symbol "\\" *> var)
  <*> (symbol "->" *> pTm 2)

pLet = Let
  <$> (symbol "let" *> var)   -- bound name
  <*> (symbol "::" *> pTm' 0)  -- type
  <*> (symbol "=" *> pTm 2)   -- value
  <*> (symbol ";" *> pTm 0)   -- scope

pApp = App <$> (pTm 5 <* ws) <*> pTm 4


pCons = brace $ Cons <$> pTm' 3 <*> (symbol "..." *> pTm' 3)

pPi = Pi 
  <$> var
  <*> (symbol ":" *> pTm' 2)
  <*> (symbol "->" *> pTm' 0)

pConstPi = ConstPi <$> pTm' 1 <*> (symbol "->" *> pTm' 0)

pTypeof = TypeOf <$> (symbol "typeof" *> pTm 4)

pDef = Def
  <$> (symbol "def" *> var)   -- type name
  <*> (symbol "::" *> pTm' 0) -- type of type
  <*> (symbol "=" *> var)   -- value
  <*> (symbol ";" *> pTm 0)   -- scope

pTm :: Int -> Parser Tm
pTm i = choice $ paren (pTm 0 <|> pTm' 0):drop i
  [
    pLet 
  , pDef
  , try pLam
  , pTypeof
  , try pApp
  , pCast
  , pVar
  ]

pTm' :: Int -> Parser Tm
pTm' i = choice $ paren (pTm' 0 <|> pTm 0):drop i
  [
    try pConstPi
  , try pPi
  , try pCons
  , pTypeof
  
  , Uni <$ symbol "{U}"
  , Top <$ (symbol "Top" <|> symbol "⊤")
  , Bot <$ (symbol "Bot" <|> symbol "⊥")
  , pVar
  ]
