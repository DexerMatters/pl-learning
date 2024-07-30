{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
module Elab where

import Value (Val, uni)


import qualified Raw as R (Ty(..), Tm(..))
import qualified Term as T (Tm(..))
import qualified Value as V (Val(..))
import Raw (Name)
import Eval (eval, ($$))
import Control.Monad (unless, when)

data TypeError 
    = BadConversion VTy VTy
    | BadConstraint VTy VTy
    | CannotInfer R.Tm

type Result = Either TypeError

type VTy = Val

data Ctx = Ctx {
    vals :: [Val]
  , sigs :: [(Name, VTy)]
  , lvl  :: Int
}

emptyCtx :: Ctx
emptyCtx = Ctx [] [] 0

define :: Name -> Val -> VTy -> Ctx -> Ctx
define n v ty ctx = ctx {
    vals = v:vals ctx
  , sigs = (n, ty):sigs ctx
  , lvl  = lvl ctx + 1
}

sub :: Ctx -> Val -> Val -> Bool -- <:
sub ctx = curry $ \case
  (_, V.Top) -> True
  (V.Bot, _) -> True
  (V.Cons t u, V.Cons t' u') 
    -> sub ctx t' t && sub ctx u u'
  (V.Pi _ ty body, V.Pi _ ty' body')
    -> sub ctx ty' ty 
    && let r = body $$ V.Var (lvl ctx)
    in let r' = body' $$ V.Var (lvl ctx)
    in sub ctx r r'
  (V.Var l, V.Var l')
    -> sub ctx (vals ctx !! l) (vals ctx !! l')
  (V.Var l, a)
    -> sub ctx (vals ctx !! l) a
  (a, V.Var l)
    -> sub ctx a (vals ctx !! l)
  _ -> False

check :: Ctx -> R.Tm -> VTy -> Result T.Tm
check ctx = curry $ \case
  (R.Lam v body, V.Pi _ ty body') -> do
    let ctx' = define v (V.Var $ lvl ctx) ty ctx
    t <- check ctx' body (body' $$ ty)
    return $ T.Lam v t
  (raw, ty) -> do
    (t, ty') <- infer ctx raw
    if sub ctx ty' ty
    then return t
    else Left $ BadConversion ty' ty


infer :: Ctx -> R.Tm -> Result (T.Tm, Val)
infer ctx = \case               -- types cannot be inferred but intepreted
  R.Bot -> pure (T.Bot, V.Bot)
  R.Top -> pure (T.Top, V.Top)
  R.Uni 
    -> pure (T.Uni, uni)
  R.Cons a b -> do
    (ta, va) <- infer ctx a
    (tb, vb) <- infer ctx b
    unless (sub ctx va vb) 
      (Left $ BadConversion va vb)
    pure (T.Cons ta tb, V.Cons va vb)
  R.Pi v tya tyb -> do
    tya' <- check ctx tya uni
