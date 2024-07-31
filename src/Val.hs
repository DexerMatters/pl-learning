module Val where
import Term (Tm)


type Lvl = Int

type Name = String

type Env = [Val]

data Closure = Closure Env Tm

data Val
  = Var Lvl              -- x
  | Lam Name Closure     -- \x.t
  | App Val Val          -- t t'
