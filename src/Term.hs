{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Term where
import Raw (Name)


type Ix = Int

type Ty = Tm

data Tm                   -- t
  = Var Ix                -- x
  | Lam Name Tm           -- \x.t
  | App Tm Tm             -- t t'
  | Let Name Ty Tm Tm     -- let x:T = t ; t'

  | Top                   -- TOP
  | Bot                   -- BOT
  | Cons Ty Ty            -- {T ... T'}
  | Uni                   -- {U}
  | Pi Name Ty Ty         -- (x:T) -> T'
