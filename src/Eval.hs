{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

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
  Let _ _ t t' -> eval (eval env t : env) t'

($$) :: Cls -> Val -> Val
(Cls env u) $$ v = eval (v : env) u

quote :: Val -> Tm
quote = \case
  VTop -> Top
  VBot -> Bot
  VPath s u -> Path (quote s) (quote u)
  VVar l _ -> Var l
  VLam n (Cls _ t) -> Lam n t
  VApp t t' -> App (quote t) (quote t')
  VPi n ty (Cls _ t) -> Pi n (quote ty) t

fresh :: Lvl -> String
fresh = pure . (unknowns !!)
  where
    unknowns = ['x', 'y', 'z'] ++ ['w' .. 'z']

nf :: Tm -> String
nf = nf' 0
  where
    nf' l = \case
      Top -> "top"
      Bot -> "bot"
      Path s u -> "{" ++ nf' l s ++ "..." ++ nf' l u ++ "}"
      Var x -> fresh (l - x - 1)
      Lam n t -> "lam " ++ n ++ ". " ++ nf' (l + 1) t
      Pi n ty t -> n ++ ":" ++ nf' l ty ++ "->" ++ nf' (l + 1) t
      Let {} -> error "Never happens"
      TyOf v -> "typeof " ++ nf' l (quote v)
      App {} -> error "Never happens"