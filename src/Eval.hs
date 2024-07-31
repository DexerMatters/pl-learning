{-# LANGUAGE LambdaCase #-}
module Eval where

import qualified Term as T (Tm(..), Ty(..))
import qualified Val as V (Val(..))
import Val (Val, Lvl, Env, Closure (Closure))
import Term (Name)


eval :: Env -> T.Tm -> Val
eval env = \case
  T.Var x -> env !! x
  T.Lam n body -> V.Lam n (Closure env body)
  T.Let _ _ t scp -> eval (eval env t:env) scp
  T.App t t' -> case eval env t of
    (V.Lam _ (Closure env' body))
      -> eval (eval env t':env') body
    _ -> error "Never reach"