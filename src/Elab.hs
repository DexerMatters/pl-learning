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


infixl 8 |-
(|-) :: a -> (a -> b) -> b
a |- f = f a


infixl 9 <:
(<:) :: Val -> Val -> (Lvl -> Bool)
(<:) = curry $ \case
  (_, VTop) -> const True
  (VBot, _) -> const True
  (VPath a b, VPath a' b') 
    -> \l -> l |- a' <: a && l |- b <: b'
  (VVar _ a, VVar _ b)    -- We only care about the type of variables
    -> \l -> l |- a <: b
  (VPi _ a f, VPi _ a' f') 
    -> \l -> let b = f $$ VVar l a'
                 b' = f' $$ VVar l a'
             in l |- a' <: a && l |- b <: b'




newVal :: Name -> Val -> VTy -> Ctx -> Ctx
newVal n v ty ctx = 
  ctx {
      env = v:env ctx
    , types = (n, ty):types ctx
    , lvl = lvl ctx + 1}

bindVal :: Name -> VTy -> Ctx -> Ctx
bindVal n ty ctx = 
  ctx { 
      env = VVar l ty:env ctx
    , types = (n, ty):types ctx
    , lvl = l + 1}
  where l = lvl ctx


-- | Check if a term has a given type
check :: Ctx -> Raw -> VTy -> Result Tm
check ctx = curry $ \case
  (RLam n raw, VPi _ ty f) ->
    let ctx' = bindVal n ty ctx
    in Lam n <$> check ctx' raw (f $$ VVar (lvl ctx) ty)
  (RLet n ty t u, ty') -> do  -- let n : ty = t ; u
    -- Make out the type value of ty
    tty <- check ctx ty uni
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
    if lvl ctx |- ty <: ty'
    then return t
    else Left $ Mismatched ty ty'


-- | Get the type of a type
iso :: VTy -> VTy
iso a = VPath a a


-- | Universe type
uni :: VTy
uni = VPath VBot VTop


-- | Infer the type of a term in a given context
infer :: Ctx -> Raw -> Result (Tm, VTy)
infer ctx = \case

  -- Lookup the type of the var and btw index the var
  RTop -> pure (Top, iso VTop)
  RBot -> pure (Bot, iso VBot)
  RPath a b -> do
    (ta, va) <- infer ctx a
    (tb, vb) <- infer ctx b
    if lvl ctx |- va <: vb
    then return $
      let va' = eval (env ctx) ta
          vb' = eval (env ctx) tb
      in (Path ta tb, iso (VPath va' vb'))
    else Left $ Mismatched va vb
  RTyOf raw -> do  -- tyOf raw
    (_, ty) <- infer ctx raw  -- We only care about the type
    return (TyOf ty, iso ty)  -- Straightly forward to evaluator
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
    tt <- check ctx t uni
    let ctx' = bindVal n (eval (env ctx) tt) ctx
    tt' <- check ctx' t' uni
    let vt = eval (env ctx) tt
    return (Pi n tt tt', iso $ VPi n vt (Cls (env ctx) tt'))
  RLam _ _ -> Left BadInference

  -- Almost identical to what we do in check
  RLet n ty t u -> do   -- let n : ty = t ; u
    tty <- check ctx ty uni
    let vty = eval (env ctx) tty
    tt <- check ctx t vty
    let vt = eval (env ctx) tt
    let ctx' = newVal n vt vty ctx
    (tu, tyu) <- infer ctx' u
    return (Let n tty tt tu, tyu)
