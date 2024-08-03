{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
module Elab where

import Value (Val, uni, Closure (Closure))


import qualified Raw as R (Ty(..), Tm(..))
import qualified Term as T (Tm(..))
import qualified Value as V (Val(..))
import Raw (Name)
import Eval (eval, ($$))
import Control.Monad (unless, when)
import Debug.Trace (trace)

data TypeError 
    = BadConversion VTy VTy
    | BadConstraint VTy VTy
    | CannotInfer
    | UnboundVariable
    | NotAFunction
    deriving Show

type Result = Either TypeError

type VTy = Val

data Ctx = Ctx {
    vals :: [Val]
  , sigs :: [(Name, VTy)]
  , lvl  :: Int
}

tr :: Show a => a -> a
tr a = trace (show a) a

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

iso :: Val -> Val
iso a = V.Cons a a

infer :: Ctx -> R.Tm -> Result (T.Tm, Val)
infer ctx = \case
  R.Bot -> pure (T.Bot, iso V.Bot)  -- X:{X...X}
  R.Top -> pure (T.Top, iso V.Top)
  R.Uni -> pure (T.Uni, iso uni)
  R.Cons a b -> do
    (ta, va) <- infer ctx a
    (tb, vb) <- infer ctx b
    pure (T.Cons ta tb, iso (V.Cons va vb))

  R.Pi x ta tb -> do
    tta <- check ctx ta uni -- check ta a type term
    let vta = eval (vals ctx) tta  -- ta may also contain programs
    let ctx' = define x (V.Var (lvl ctx)) vta ctx
    ttb <- check ctx' tb uni  -- check tb a type term
    let cls = Closure (vals ctx') ttb
    return 
      (T.Pi x tta ttb, iso $ V.Pi x vta cls)
  R.ConstPi ta tb -> do
    tta <- check ctx ta uni
    let vta = eval (vals ctx) tta
    ttb <- check ctx tb uni
    let cls = Closure (vals ctx) ttb
    return 
      (T.Pi "" tta ttb, iso $ V.Pi "" vta cls)  -- bound name is trival
  R.TypeOf t -> do
    (tt, vt) <- infer ctx t
    case vt of
      V.Var l -> return (T.TypeOf l, eval (vals ctx) tt)
      _ -> return (tt, iso vt)
  -- Below are terms that behave as values
  R.Lam {} -> Left CannotInfer
  R.Cast ty tm -> do
    tty <- check ctx ty uni
    let vty = eval (vals ctx) tty
    ttm <- check ctx tm vty
    return (ttm, vty)
  R.Var n -> 
    let go _ [] = Left UnboundVariable
        go i ((n', ty):rest)
          | n' == n = return (T.Var i, ty)
          | otherwise = go (i + 1) rest
    in go 0 (sigs ctx)
  R.App a b -> do   -- ta can only be presented as variable
    (ta, va) <- infer ctx a
    case va of
      (V.Pi _ vb' body) -> do
        tb <- check ctx b vb'
        return (T.App ta tb, body $$ eval (vals ctx) tb)
      _ -> Left NotAFunction
  R.Let x ty t scp -> do  -- let x::ty = t; scp
    tty <- check ctx ty uni
    let vty = eval (vals ctx) tty
    tt <- check ctx t vty
    let vt = eval (vals ctx) tt
    let ctx' = define x vt vty ctx
    (tscp, vscp) <- infer ctx' scp
    return (T.Let x tty tt tscp, vscp)
  R.Def n ty n' scp -> do  -- def n::ty = n'; scp
    tty <- check ctx ty uni
    let vty = eval (vals ctx) tty
    let ctx' = define n (V.Var (lvl ctx)) vty ctx
    let ctx'' = define n' (V.Con n') (V.Var (lvl ctx)) ctx'
    (tscp, vscp) <- infer ctx'' scp
    return (T.Def n tty n' tscp, vscp)
