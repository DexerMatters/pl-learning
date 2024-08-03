{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE InstanceSigs #-}
module Value where
import Raw (Name)
import Term (Tm, Ix)


type Env = [Val]

data Closure = Closure Env Tm deriving (Show)

uni :: Val
uni = Cons Bot Top

data Val                    -- t
  = Var Ix                  -- x
  | Lam Name Closure        -- \x.t
  | Top                     -- TOP
  | Bot                     -- BOT
  | Cons Val Val            -- {T ... T'}
  | Pi Name Val Closure     -- (x:T) -> T'
  | Con Name                -- X

instance Show Val where
  show :: Val -> String
  show (Var x) = "x%" ++ show x
  show (Lam n _) = "\\" ++ n ++ "->" ++ "<Closure>"
  show Top = "⊤"
  show Bot = "⊥"
  show (Cons v1 v2) = "{" ++ show v1 ++ "..." ++ show v2 ++"}"
  show (Pi n v _) = n ++ ":" ++ show v ++ "->" ++ "<Closure>"
  show (Con n) = n