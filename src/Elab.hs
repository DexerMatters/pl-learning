{-# LANGUAGE LambdaCase #-}
module Elab where


import qualified Term as T (Tm(..), Ty(..))
import qualified Raw as R (Tm(..), Ty(..))
import Term (Name)
import Control.Monad (unless)
import Val (Lvl)


data Ctx = Ctx {
  valTys :: [(Name, T.Ty)],
  defs   :: [(Name, T.Ty)],
  lvl    :: Lvl
}

emptyCtx :: Ctx
emptyCtx = Ctx [] [] 0

data TypeError
  = BadConverison T.Ty T.Ty
  | NotFromPath
  | NotAFunction
  | BadConstraint T.Ty T.Ty
  | UndefinedType Name
  | UnboundVariable Name
  | BadInference
  deriving Show

type Result = Either TypeError

def :: Name -> T.Ty -> Ctx -> Ctx
def n ty ctx = ctx { 
    defs = (n, ty):defs ctx 
}

bind :: Name -> T.Ty -> Ctx -> Ctx
bind n ty ctx = ctx {
    valTys = (n, ty):valTys ctx
  , lvl    = lvl ctx + 1
}

-- Subtyping ... (Judge conversibility of two types)
-- Note : As validating extends the infomation of every type,
--        there's no need for contexts to store relations
infixl 8 <:
(<:) :: T.Ty -> T.Ty -> Bool
(<:) = curry $ \case
  (_, T.Top) -> True
  (T.Bot, _) -> True
  (T.Arro a b, T.Arro a' b') -- contravariance
    -> a' <: a && b <: b'
  (T.Path a b, T.Path a' b') -- narrowing
    -> a' <: a && b <: b'
  (s, T.Path a b)
    -> s <: b && a <: s 
  _ -> False

valid :: Ctx -> R.Ty -> Result T.Ty
valid ctx = \case
  R.Top -> pure T.Top
  R.Bot -> pure T.Bot
  R.Path a b -> do
    a' <- valid ctx a
    b' <- valid ctx b
    unless (a' <: b') 
      (Left $ BadConstraint a' b')
    return (T.Path a' b')
  R.Arro a b -> T.Arro 
    <$> valid ctx a 
    <*> valid ctx b
  R.TVar n -> case lookup n (defs ctx) of
    Just t -> return t
    Nothing -> Left $ UndefinedType n
  
  {- Sugar -}
  R.Iso ty -> T.Path 
    <$> valid ctx ty 
    <*> valid ctx ty
  R.Poly -> pure $ T.Path T.Bot T.Top
  
-- There is no attendence of values
infer :: Ctx -> R.Tm -> Result (T.Tm, T.Ty)
infer ctx = \case
  R.Cast ty t -> do
    tty <- valid ctx ty
    tt <- check ctx t tty
    return (tt, tty)
  R.Var n -> 
    let go _ [] = Left $ UnboundVariable n
        go i ((n', ty):rest)
          | n == n' = return (T.Var i, ty)
          | otherwise = go (i + 1) rest
    in go 0 (valTys ctx)
  R.Lam {} -> Left BadInference
  R.Let n ty t scp -> do
    ty' <- valid ctx ty
    t' <- check ctx t ty'
    let ctx' = bind n ty' ctx
    (scp', scpT) <- infer ctx' scp
    return (T.Let n ty' t' scp', scpT)
  R.Def n ty scp -> do
    ty' <- valid ctx ty
    infer (def n ty' ctx) scp
  R.Def' n a b  scp -> do
    a' <- valid ctx a
    b' <- valid ctx b
    infer (def n (T.Free a' b') ctx) scp
    
  -- Application can only be valid when function is presented as Variable
  R.App f a -> do
    (ft, fT) <- infer ctx f
    case fT of
      T.Arro aT body -> do
        at <- check ctx a aT
        return (T.App ft at, body)
      _ -> Left NotAFunction
    

check :: Ctx -> R.Tm -> T.Ty -> Result T.Tm
check ctx = curry $ \case
  -- Only lambda can be checked
  (R.Lam n body, T.Arro ty ty') -> do
    let ctx' = bind n ty ctx
    bodyt <- check ctx' body ty'
    return $ T.Lam n bodyt

  -- Change direction
  (t, ty) -> do
    (t', ty') <- infer ctx t
    unless (ty' <: ty) 
      (Left $ BadConverison ty' ty)
    return t'