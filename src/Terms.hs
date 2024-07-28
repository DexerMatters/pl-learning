module Terms where


type Name = String

type Ix = Int
type Lvl = Int

data Raw
  = RU                      -- U
  | RVar Name               -- x
  | RLam Name Raw           -- \x -> t
  | RApp Raw Raw            -- t u
  | RPi Name Raw Raw        -- (x:t) -> u
  | RLet Name Raw Raw Raw   -- let x:t = u; v


type Ty = Tm

data Tm
  = U
  | Var Ix
  | Lam Name Tm
  | App Tm Tm
  | Pi Name Tm Tm
  | Let Name Tm Tm Tm


type VTy = Val
type Env = [Val]

data Cls = Cls Env Tm

data Val
  = VU
  | VVar Lvl
  | VLam Name Cls
  | VApp Val Val
  | VPi Name VTy Cls

