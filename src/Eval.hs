{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE LambdaCase #-}
module Eval where
import Terms


eval :: Env -> Tm -> Val
eval env = \case
  Top -> VTop
  Bot -> VBot
  Path s u -> VPath (eval env s) (eval env u)
  TyOf v -> v
  Var x -> env !! x
  Lam n t -> VLam n (Cls env t)
  App t t' -> case (eval env t, eval env t') of
                (VLam _ f, a) -> f $$ a
                (f, a) -> VApp f a
  Pi n ty t -> VPi n (eval env ty) (Cls env t)
  Let _ _ t t' -> eval (eval env t:env) t'


($$) :: Cls -> Val -> Val
(Cls env u) $$ v = eval (v:env) u