{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
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
deriveForDec className _ f (InstanceD overlaps cxt classHead decs) = do
    let (givenClassName, firstParam : _) = classHeadToParams classHead
    when (givenClassName /= className) $
      fail $ "while deriving " ++ show className ++ ": wrong class name in prototype declaration: " ++ show givenClassName
    let dataTypeName = headOfType firstParam
    dataTypeInfo <- reify dataTypeName
    case dataTypeInfo of
        TyConI (DataD dataCxt name bndrs _ cons _) -> do
            dec <- f bndrs cons
            return [InstanceD overlaps cxt classHead [dec]]
        _ -> fail $ "while deriving " ++ show className ++ ": the name of an algebraic data type constructor is required"
deriveForDec className makeClassHead f (DataD dataCxt name bndrs _ cons _) = return <$> inst
    where
        inst = instanceD (cxt (map return dataCxt)) (makeClassHead $ conT name) [dec]
        dec = f bndrs cons
deriveForDec className makeClassHead f (DataInstD dataCxt name tyArgs _ cons _) = return <$> inst
    where
        inst = instanceD (cxt (map return dataCxt)) (makeClassHead $ foldl1 appT (map return $ (ConT name : init tyArgs))) [dec]
        -- TODO: figure out proper number of family parameters vs instance parameters
        bndrs = [PlainTV v | VarT v <- tail tyArgs ]
        dec = f bndrs cons
