{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.TH.Utils where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

punt :: Show a => a -> ExpQ
punt = litE . stringL . show

-- various constants and utility functions

nameOfCon (NormalC  name _) = name
nameOfCon (RecC     name _) = name
nameOfCon (InfixC _ name _) = name
nameOfCon (ForallC _ _ con) = nameOfCon con

argCountOfCon (NormalC  _ args) = length args
argCountOfCon (RecC     _ args) = length args
argCountOfCon (InfixC _ _ _)    = 2
argCountOfCon (ForallC _ _ con) = argCountOfCon con

-- WARNING: does not handle ForallC in any kind of generally-applicable way!
argTypesOfCon (NormalC  _ args) = map snd args
argTypesOfCon (RecC     _ args) = [t | (_,_,t) <- args]
argTypesOfCon (InfixC x _ y)    = map snd [x,y]
argTypesOfCon (ForallC _ _ con) = argTypesOfCon con

varsBoundInCon (ForallC bndrs _ con) = bndrs ++ varsBoundInCon con
varsBoundInCon _ = []

occursInType :: TyVarBndr -> Type -> Bool
occursInType bndr ty = case ty of
        ForallT bndrs _ ty
            | any (== nameOfBinder bndr) (map nameOfBinder bndrs)
                -> False
            | otherwise
                -> occursInType bndr ty
        VarT name
            | name == nameOfBinder bndr -> True
            | otherwise                 -> False
        AppT ty1 ty2 -> occursInType bndr ty1 || occursInType bndr ty2
        SigT ty _ -> occursInType bndr ty
        _ -> False

nameOfBinder (PlainTV name)     = name
nameOfBinder (KindedTV name _)  = name

headOfType :: Type -> Name
headOfType (ForallT _ _ ty) = headOfType ty
headOfType (VarT name) = name
headOfType (ConT name) = name
headOfType (TupleT n) = tupleTypeName n
headOfType (UnboxedTupleT n) = unboxedTupleTypeName n -- error "don't know how to get the name of an unboxed tuple type"
headOfType ArrowT = ''(->)
headOfType ListT = ''[]
headOfType (AppT t _) = headOfType t
headOfType (SigT t _) = headOfType t

compose [] = [| id |]
compose [f] = f
compose (f:fs) = [| $f . $(compose fs) |]
