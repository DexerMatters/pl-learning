module Raw where


type Name = String

data Tm
  = Cast Ty Tm           -- [T]t
  | Var Name            -- x
  | Lam Name Tm         -- \x.t
  | Let Name Ty Tm Tm   -- let x : T = t in t'
  | Def Name Ty Tm      -- def A = T; t
  | App Tm Tm           -- t t'
  deriving Show


data Ty
  = Top
  | Bot
  | Path Ty Ty    -- {A ... B}
  | Arro Ty Ty    -- T -> T'
  | TVar Name     -- T

  {- Sugar -}
  | Iso Ty        -- {T ... T}
  | Poly          -- {Bot ... Top}
  deriving Show