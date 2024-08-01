module Parser(Parser, pTm) where
import Text.Megaparsec (Parsec, many, (<|>), some, between, choice, MonadParsec (try))
import Data.Void (Void)
import Raw (Tm (..), Ty (..))
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C


type Parser = Parsec Void String

type ParseStr = Parser String

pres :: [String]
pres = ["let", "def"]

-- Basic Parsers
ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: ParseStr -> ParseStr
lexeme = L.lexeme ws

symbol :: String -> ParseStr
symbol = L.symbol ws

var :: ParseStr
var = lexeme $ ((:) <$> C.lowerChar) <*> many C.letterChar

tvar :: ParseStr
tvar = lexeme $ ((:) <$> C.upperChar) <*> many C.letterChar

paren :: Parser a -> Parser a
paren p = C.char '(' *> p <* C.char ')'

bracket :: Parser a -> Parser a
bracket p = C.char '[' *> p <* C.char ']'

brace :: Parser a -> Parser a
brace p = C.char '{' *> p <* C.char '}'

-- AST Parsers

pVar, pCast, pLam, pLet, pDef, pApp :: Parser Tm
pVar = Var <$> var

pCast = Cast <$> bracket (pTy 0) <*> pTm 5

pLam = Lam 
  <$> (symbol "\\" *> var)
  <*> (symbol "." *> pTm 2)

pLet = Let
  <$> (symbol "let" *> var)   -- bound name
  <*> (symbol ":" *> pTy 0)     -- type
  <*> (symbol "=" *> pTm 2)   -- value
  <*> (symbol ";" *> pTm 0)   -- scope

pDef = Def
  <$> (symbol "def" *> tvar)  -- type name
  <*> (symbol "=" *> pTy 0)     -- implied type
  <*> (symbol ";" *> pTm 0)   -- scope

pApp = App <$> (pTm 4 <* ws) <*> pTm 3


pTm :: Int -> Parser Tm
pTm i = choice $ paren (pTm 0):drop i
  [
    pDef
  , pLet
  , pLam
  , try pApp
  , pCast
  , pVar
  ]

pPath, pArro, pTVar :: Parser Ty
pPath = brace (Path <$> pTy 1 <*> (symbol "..." *> pTy 1))

pArro = Arro <$> (pTy 1 <* symbol "->") <*> pTy 0

pTVar = TVar <$> tvar


pTy :: Int -> Parser Ty
pTy i = choice $ paren (pTy 0):drop i
  [
    try pArro
  , pPath
  , Poly <$ symbol "X"
  , Top <$ symbol "Top"
  , Bot <$ symbol "Bot"
  , pTVar
  ]