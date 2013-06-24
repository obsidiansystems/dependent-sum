{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |Separate module for Typeable declaration, to minimize the amount of
-- visual inspection required to determine that this package is "safe"
module Data.Dependent.Sum.Typeable where

import {-# SOURCE #-} Data.Dependent.Sum
import Data.Typeable


instance (Typeable1 t, Typeable1 f) => Typeable (DSum t f) where
    typeOf ds = mkTyConApp dSumCon [typeOfF, typeOfT]
        where
            typeOfF = typeOf1 $ (undefined :: DSum f t -> f a) ds
            typeOfT = typeOf1 $ (undefined :: DSum f t -> t a) ds

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
            dSumCon = mkTyCon3 "dependent-sum" "Data.Dependent.Sum" "DSum"
#else 
            dSumCon = mkTyCon "Data.Dependent.Sum.DSum"
#endif