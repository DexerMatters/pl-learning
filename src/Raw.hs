{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Raw where


type Name = String

type Ty = Tm

data Tm                   -- t
  = Var Name              -- x
  | Lam Name Tm           -- \x.t
  | App Tm Tm             -- t t'
  | Def Name Ty Tm        -- def T = T'; t'
  | Let Name Ty Tm Tm     -- let x:T = t ; t'
  
  | Top                   -- TOP
  | Bot                   -- BOT
  | Cons Ty Ty            -- {T ... T'}
  | Uni                   -- {U}
  | Pi Name Ty Ty         -- (x:T) -> T'
