{-# LANGUAGE LambdaCase #-}
module Elab where
import Terms
import Eval (($$), eval)


type Tys = [(Name, VTy)]

type Result = Either TypeError

data Ctx = Ctx { env :: Env, types :: Tys, lvl :: Lvl }

data TypeError 
  = Undefined Name 
  | Mismatched VTy VTy
  | ExpectedFunction VTy
  | BadInference

conv :: Lvl -> Val -> Val -> Bool
conv l = curry $ \case
  (VU, VU) -> True
  (VVar lv, VVar lv') -> lv == lv'

  (VLam _ f, VLam _ f') 
    -> conv (l + 1) (f $$ VVar l) (f' $$ VVar l)
  (VLam _ f, v) 
    -> conv (l + 1) (f $$ VVar l) v
  (v, VLam _ f) 
    -> conv (l + 1) v (f $$ VVar l)

  (VApp a b, VApp a' b') 
    -> conv l a a' && conv l b b'
  (VPi _ ty f, VPi _ ty' f')
    -> conv l ty ty' 
    && conv (l + 1) (f $$ VVar l) (f' $$ VVar l)
  _ -> False


newVal :: Name -> Val -> VTy -> Ctx -> Ctx
newVal n v ty ctx = 
  ctx {
      env = v:env ctx
    , types = (n, ty):types ctx
    , lvl = lvl ctx + 1}

bindVal :: Name -> VTy -> Ctx -> Ctx
bindVal n ty ctx = 
  ctx { 
      env = VVar l:env ctx
    , types = (n, ty):types ctx
    , lvl = l + 1}
  where l = lvl ctx

check :: Ctx -> Raw -> VTy -> Result Tm
check ctx = curry $ \case
  (RLam n raw, VPi _ ty f) ->
    let ctx' = bindVal n ty ctx
    in Lam n <$> check ctx' raw (f $$ VVar (lvl ctx))
  (RLet n ty t u, ty') -> do  -- let n : ty = t ; u
    -- Make out the type value of ty
    tty <- check ctx ty VU
    let vty = eval (env ctx) tty

    -- Make out the type value of t. Check it with vty
    tt <- check ctx t vty
    let vt = eval (env ctx) tt

    -- Update context by adding a new n-named variable
    -- Check the type of u with new variable
    let ctx' = newVal n vt vty ctx
    tu <- check ctx' u ty'

    return (Let n tty tt tu)
  
  -- Other cases: Use inference to match types
  (raw, ty) -> do
    (t, ty') <- infer ctx raw
    if conv (lvl ctx) ty ty'
    then return t
    else Left $ Mismatched ty ty'


infer :: Ctx -> Raw -> Result (Tm, VTy)
infer ctx = \case
  -- Lookup the type of the var and btw index the var
  RU -> pure (U, VU)
  RVar n -> 
    let go _ [] = Left $ Undefined n
        go i ((n', ty):tys)
          | n == n'   = return (Var i, ty)
          | otherwise = go (i + 1) tys
    in go 0 (types ctx)
  RApp f a -> do  -- f a
    (tf, vf) <- infer ctx f
    case vf of
      VPi _ tya cls -> do
        ta <- check ctx a tya
        return (App tf ta, cls $$ eval (env ctx) ta)
      _ -> Left $ ExpectedFunction vf 
  RPi n t t' -> do  -- (x:t) -> u
    tt <- check ctx t VU
    let ctx' = bindVal n (eval (env ctx) tt) ctx
    tt' <- check ctx' t' VU
    return (Pi n tt tt', VU)
  RLam _ _ -> Left BadInference

  -- Almost identical to what we do in check
  RLet n ty t u -> do   -- let n : ty = t ; u
    tty <- check ctx ty VU
    let vty = eval (env ctx) tty
    tt <- check ctx t vty
    let vt = eval (env ctx) tt
    let ctx' = newVal n vt vty ctx
    (tu, tyu) <- infer ctx' u
    return (Let n tty tt tu, tyu)
  
 