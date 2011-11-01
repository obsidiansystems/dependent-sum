{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Data.GADT.Compare.TH
    ( DeriveGEQ(..)
    , DeriveGCompare(..)
    ) where

import Control.Monad
import Data.GADT.Compare
import Language.Haskell.TH

punt :: Show a => a -> ExpQ
punt = litE . stringL . show

-- A type class purely for overloading purposes
class DeriveGEQ t where
    deriveGEq :: t -> Q [Dec]

instance DeriveGEQ Name where
    deriveGEq typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGEq dec
            _ -> fail "deriveGEq: the name of a type constructor is required"

instance DeriveGEQ Dec where
    deriveGEq (DataD dataCxt name bndrs cons _) = 
        fmap return inst
        
        where
            geqClause con = do
                let argTypes = argTypesOfCon con
                    needsGEq argType = any (`occursInType` argType) (bndrs ++ varsBoundInCon con)
                    
                    nArgs = length argTypes
                lArgNames <- replicateM nArgs (newName "x")
                rArgNames <- replicateM nArgs (newName "y")
                
                clause [ conP conName (map varP lArgNames)
                       , conP conName (map varP rArgNames)
                       ]
                    ( normalB $ doE 
                        (  [ if needsGEq argType
                                then bindS (conP 'Refl []) [| geq $(varE lArg) $(varE rArg) |]
                                else noBindS [| guard ($(varE lArg) == $(varE rArg)) |]
                           | (lArg, rArg, argType) <- zip3 lArgNames rArgNames argTypes
                           ]
                        ++ [ noBindS [| return Refl |] ]
                        )
                    ) []
                where conName = nameOfCon con
            
            inst = instanceD (cxt (map return dataCxt)) (appT (conT ''GEq) (conT name)) [geqDec]
            geqDec = funD 'geq 
                (  map geqClause cons
                ++  [ clause [wildP, wildP] (normalB [| Nothing |]) [] 
                    | length cons /= 1
                    ]
                )

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

-- A monad allowing gcompare to be defined in the same style as geq
newtype GComparing a b t = GComparing (Either (GOrdering a b) t)

instance Functor (GComparing a b) where fmap f (GComparing x) = GComparing (either Left (Right . f) x)
instance Monad (GComparing a b) where
    return = GComparing . Right
    GComparing (Left  x) >>= f = GComparing (Left x)
    GComparing (Right x) >>= f = f x

geq' :: GCompare t => t a -> t b -> GComparing x y (a := b)
geq' x y = GComparing $ case gcompare x y of
    GLT -> Left GLT
    GEQ -> Right Refl
    GGT -> Left GGT

compare' x y = GComparing $ case compare x y of
    LT -> Left GLT
    EQ -> Right ()
    GT -> Left GGT

runGComparing (GComparing x) = either id id x

class DeriveGCompare t where
    deriveGCompare :: t -> Q [Dec]

instance DeriveGCompare Name where
    deriveGCompare typeName = do
        typeInfo <- reify typeName
        case typeInfo of
            TyConI dec -> deriveGCompare dec
            _ -> fail "deriveGCompare: the name of a type constructor is required"

instance DeriveGCompare Dec where
    deriveGCompare (DataD dataCxt name bndrs cons _) = 
        fmap return inst
        
        where
            -- for every constructor, first check for equality (recursively comparing 
            -- arguments) then add catch-all cases; all not-yet-matched patterns are
            -- "greater than" the constructor under consideration.
            gcompareClauses con = 
                [ mainClause con
                , clause [recP conName [], wildP] (normalB [| GLT |]) []
                , clause [wildP, recP conName []] (normalB [| GGT |]) []
                ] where conName = nameOfCon con
            
            -- main clause; using the 'GComparing' monad, compare all arguments to the 
            -- constructor recursively, attempting to unify type variables by recursive 
            -- calls to gcompare whenever needed (that is, whenever a constructor argument's
            -- type contains a variable bound in the data declaration or in the constructor's
            -- type signature)
            mainClause con = do
                let argTypes = argTypesOfCon con
                    needsGCompare argType = any (`occursInType` argType) (bndrs ++ varsBoundInCon con)
                    
                    nArgs = length argTypes
                lArgNames <- replicateM nArgs (newName "x")
                rArgNames <- replicateM nArgs (newName "y")
                
                clause [ conP conName (map varP lArgNames)
                       , conP conName (map varP rArgNames)
                       ]
                    ( normalB [|
                        runGComparing $ $(doE 
                            (  [ if needsGCompare argType
                                    then bindS (conP 'Refl []) [| geq' $(varE lArg) $(varE rArg) |]
                                    else noBindS [| compare' $(varE lArg) $(varE rArg) |]
                               | (lArg, rArg, argType) <- zip3 lArgNames rArgNames argTypes
                               ]
                            ++ [ noBindS [| return GEQ |] ]
                            ))
                        |]
                    ) []
                where conName = nameOfCon con
            
            inst = instanceD (cxt (map return dataCxt)) (appT (conT ''GCompare) (conT name)) [gcompareDec]
            gcompareDec
                | null cons = funD 'gcompare [clause [bangP wildP, bangP wildP] (normalB [| undefined |]) []]
                | otherwise = funD 'gcompare (concatMap gcompareClauses cons)

