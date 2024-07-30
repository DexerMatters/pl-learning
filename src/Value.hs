{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Value where
import Raw (Name)
import Term (Tm, Ix)


type Env = [Val]

data Closure = Closure Env Tm

uni :: Val
uni = Cons Bot Top

data Val                    -- t
  = Var Ix                  -- x
  | Lam Name Closure        -- \x.t
  | Top                     -- TOP
  | Bot                     -- BOT
  | Cons Val Val            -- {T ... T'}
  | Pi Name Val Closure     -- (x:T) -> T'

  | App Val Val
