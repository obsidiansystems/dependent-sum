{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PolyKinds #-}
#endif
-- | Shared functions for dependent-sum-template
module Data.Dependent.Sum.TH.Internal where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Extras

classHeadToParams :: Type -> (Name, [Type])
classHeadToParams t = (h, reverse reversedParams)
  where (h, reversedParams) = go t
        go :: Type -> (Name, [Type])
        go t = case t of
          AppT f x ->
            let (h, reversedParams) = classHeadToParams f
            in (h, x : reversedParams)
          _ -> (headOfType t, [])

-- Invoke the deriver for the given class instance.  We assume that the type
-- we're deriving for is always the first typeclass parameter, if there are
-- multiple.
deriveForDec :: Name -> (Q Type -> Q Type) -> ([TyVarBndr] -> [Con] -> Q Dec) -> Dec -> Q [Dec]
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
deriveForDec className _ f (InstanceD overlaps cxt classHead decs) = do
#else
deriveForDec className _ f (InstanceD cxt classHead decs) = do
#endif
    let (givenClassName, firstParam : _) = classHeadToParams classHead
    when (givenClassName /= className) $
      fail $ "while deriving " ++ show className ++ ": wrong class name in prototype declaration: " ++ show givenClassName
    let dataTypeName = headOfType firstParam
    dataTypeInfo <- reify dataTypeName
    case dataTypeInfo of
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
        TyConI (DataD dataCxt name bndrs _ cons _) -> do
#else
        TyConI (DataD dataCxt name bndrs cons _) -> do
#endif
            dec <- f bndrs cons
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
            return [InstanceD overlaps cxt classHead [dec]]
#else
            return [InstanceD cxt classHead [dec]]
#endif
        _ -> fail $ "while deriving " ++ show className ++ ": the name of an algebraic data type constructor is required"
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
deriveForDec className makeClassHead f (DataD dataCxt name bndrs _ cons _) = return <$> inst
#else
deriveForDec className makeClassHead f (DataD dataCxt name bndrs cons _) = return <$> inst
#endif
    where
        inst = instanceD (cxt (map return dataCxt)) (makeClassHead $ conT name) [dec]
        dec = f bndrs cons
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 612
#if __GLASGOW_HASKELL__ >= 800
deriveForDec className makeClassHead f (DataInstD dataCxt name tyArgs _ cons _) = return <$> inst
#else
deriveForDec className makeClassHead f (DataInstD dataCxt name tyArgs cons _) = return <$> inst
#endif
    where
        inst = instanceD (cxt (map return dataCxt)) (makeClassHead $ foldl1 appT (map return $ (ConT name : init tyArgs))) [dec]
        -- TODO: figure out proper number of family parameters vs instance parameters
        bndrs = [PlainTV v | VarT v <- tail tyArgs ]
        dec = f bndrs cons
#endif
