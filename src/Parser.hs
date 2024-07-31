module Parser where
import Text.Megaparsec (Parsec, many, (<|>), some, between, choice)
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
var = lexeme $ ((:) <$> C.lowerChar) <*> some C.letterChar

tvar :: ParseStr
tvar = lexeme $ ((:) <$> C.upperChar) <*> some C.letterChar

paren :: Parser a -> Parser a
paren p = C.char '(' *> p <* C.char ')'

bracket :: Parser a -> Parser a
bracket p = C.char '[' *> p <* C.char ']'

-- AST Parsers

pVar, pCast, pLam, pLet, pDef, pApp :: Parser Tm
pVar = Var <$> var

pCast = Cast <$> paren pTy <*> pTm 0

pLam = Lam 
  <$> (symbol "\\" *> var)
  <*> (symbol "." *> pTm 0)

pLet = Let
  <$> (symbol "let" *> var)   -- bound name
  <*> (symbol ":" *> pTy)     -- type
  <*> (symbol "=" *> pTm 0)   -- value
  <*> (symbol ";" *> pTm 0)   -- scope

pDef = Def
  <$> (symbol "def" *> tvar)  -- type name
  <*> (symbol "=" *> pTy)     -- implied type
  <*> (symbol ";" *> pTm 0)   -- scope

pApp = App <$> pTm 0 <*> pTm 0


pTm :: Int -> Parser Tm
pTm i = choice $ paren (pTm 0):drop i
  [
    pVar
  , pCast
  ]

pTy :: Parser Ty
pTy = pure Top