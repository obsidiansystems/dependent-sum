{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared functions for dependent-sum-template
module Data.Dependent.Sum.TH.Internal where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Language.Haskell.TH.Datatype

import Data.Maybe
import Control.Monad
import Control.Monad.Writer
import Language.Haskell.TH
import Language.Haskell.TH.Extras
import Data.Set (Set)
import qualified Data.Set as Set

-- Do not export this type family, it must remain empty. It's used as a way to trick GHC into not unifying certain type variables.
type family Skolem :: k -> k

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName = \case
  PlainTV n -> n
  KindedTV n _ -> n

skolemize :: Set Name -> Type -> Type
skolemize rigids t = case t of
  ForallT bndrs cxt t' -> ForallT bndrs cxt (skolemize (Set.difference rigids (Set.fromList (map tyVarBndrName bndrs))) t')
  AppT t1 t2 -> AppT (skolemize rigids t1) (skolemize rigids t2)
  SigT t k -> SigT (skolemize rigids t) k
  VarT v -> if Set.member v rigids
    then AppT (ConT ''Skolem) (VarT v)
    else t
  InfixT t1 n t2 -> InfixT (skolemize rigids t1) n (skolemize rigids t2)
  UInfixT t1 n t2 -> UInfixT (skolemize rigids t1) n (skolemize rigids t2)
  ParensT t -> ParensT (skolemize rigids t)
  _ -> t

reifyInstancesWithRigids :: Set Name -> Name -> [Type] -> Q [InstanceDec]
reifyInstancesWithRigids rigids cls tys = reifyInstances cls (map (skolemize rigids) tys)

-- | Determine the type variables which occur freely in a type.
freeTypeVariables :: Type -> Set Name
freeTypeVariables t = case t of
  ForallT bndrs _ t' -> Set.difference (freeTypeVariables t') (Set.fromList (map nameOfBinder bndrs))
  AppT t1 t2 -> Set.union (freeTypeVariables t1) (freeTypeVariables t2)
  SigT t _ -> freeTypeVariables t
  VarT n -> Set.singleton n
  _ -> Set.empty

subst :: Map Name Type -> Type -> Type
subst s = f
  where
    f = \case
      ForallT bndrs cxt t -> 
        let s' = Map.difference s (Map.fromList [(k,()) | k <- map tvName bndrs])
        in ForallT bndrs cxt (subst s' t)
      AppT t t' -> AppT (f t) (f t')
      SigT t k -> SigT (f t) k
      VarT n -> case Map.lookup n s of
        Just t -> t
        Nothing -> VarT n
      InfixT t x t' -> InfixT (f t) x (f t')
      UInfixT t x t' -> UInfixT (f t) x (f t')
      x -> x

-- Invoke the deriver for the given class instance.  We assume that the type
-- we're deriving for is always the first typeclass parameter, if there are
-- multiple.
deriveForDec
  :: Name
  -> (Q Type -> Q Type)
  -> ([TyVarBndr] -> [Con] -> WriterT [Type] Q Dec)
  -> Dec
  -> Q [Dec]
deriveForDec className _ f (InstanceD overlaps cxt classHead decs) = do
  let (givenClassName, firstParam : _) = classHeadToParams classHead
  when (givenClassName /= className) $
    fail $ "while deriving " ++ show className ++ ": wrong class name in prototype declaration: " ++ show givenClassName
  let dataTypeName = headOfType firstParam
  dataTypeInfo <- reify dataTypeName
  case dataTypeInfo of
    TyConI (DataD dataCxt name bndrs _ cons _) -> do
      (dec, cxt') <- runWriterT $ f bndrs cons
      return [InstanceD overlaps (cxt ++ cxt') classHead [dec]]
    _ -> fail $ "while deriving " ++ show className ++ ": the name of an algebraic data type constructor is required"
deriveForDec className makeClassHead f (DataD dataCxt name bndrs _ cons _) = do
  (dec, cxt') <- runWriterT (f bndrs cons)
  inst <- instanceD (pure (dataCxt ++ cxt')) (makeClassHead $ conT name) [pure dec]
  return [inst]
deriveForDec className makeClassHead f (DataInstD dataCxt name tyArgs _ cons _) = do
  let bndrs = [PlainTV v | VarT v <- tail tyArgs ]
  (dec, cxt') <- runWriterT (f bndrs cons)
  inst <- instanceD (pure (dataCxt ++ cxt')) (makeClassHead $ foldl1 appT (map return $ (ConT name : init tyArgs))) [pure dec]
  return [inst]

classHeadToParams :: Type -> (Name, [Type])
classHeadToParams t = (h, reverse reversedParams)
  where
    (h, reversedParams) = go t
    go :: Type -> (Name, [Type])
    go t = case t of
      AppT f x ->
        let (h, reversedParams) = classHeadToParams f
        in (h, x : reversedParams)
      _ -> (headOfType t, [])

makeTopVars :: Name -> Q [Name]
makeTopVars tyConName = do
  (tyVarBndrs, kArity) <- tyConArity' tyConName
  extraVars <- replicateM kArity (newName "topvar")
  return (map nameOfBinder tyVarBndrs ++ extraVars)