{-# LANGUAGE LambdaCase #-}
module Eval where

import qualified Value as V (Val(..)) 
import qualified Term as T (Tm(..)) 
import Value (Env, Val, Closure (Closure), uni)
import Term (Tm)


eval :: Env -> Tm -> Val
eval env = \case 
  T.Bot -> V.Bot
  T.Top -> V.Top
  T.Uni -> uni
  T.Cons ty ty' 
    -> V.Cons (eval env ty) (eval env ty')
  T.Var x -> env !! x
  T.Lam n body
    -> V.Lam n $ Closure env body
  T.App f a -> case eval env f of
    (V.Lam _ body) -> body $$ eval env a
    _ -> error "Never reach"
  T.Let _ _ t t'     -- let n:ty = t; t'
    -> eval (eval env t:env) t'
  T.Pi n ty ty'      -- (x:T) -> T'
    -> V.Pi n (eval env ty) $ Closure env ty'
      

($$) :: Closure -> Val -> Val
(Closure env tm) $$ v = eval (v:env) tm