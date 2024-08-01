module Term where


type Ix = Int

type Name = String

data Tm
  = Var Ix              -- x
  | Lam Name Tm         -- \x.t
  | Let Name Ty Tm Tm   -- let x : T = t; t'
  | App Tm Tm           -- t t'
  deriving Show


data Ty
  = Top
  | Bot
  | Path Ty Ty    -- {A ... B}
  | Arro Ty Ty    -- T -> T'
  deriving Show