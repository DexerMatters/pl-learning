module Terms where

type Variable = String

type TypeName = String

data ExprTy = 
      LowerBound
    | UpperBound
    | Concrete      TypeName

    | TyAbs         Variable ExprTy ExprTy
    | TyOf          Variable
    | TyIns         ExprTy ExprTy


data ExprTm = 
      Variable      Variable
    | TyValue       ExprTy
    | Abstr         Variable ExprTy ExprTm
    | Apply         ExprTm ExprTm

