{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |Separate module for Typeable declaration, to minimize the amount of
-- visual inspection required to determine that this package is "safe"
module Data.Dependent.Sum.Typeable where

import {-# SOURCE #-} Data.Dependent.Sum
import Data.Typeable

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702

instance Typeable1 t => Typeable (DSum t) where
    typeOf ds = mkTyConApp dSumCon [typeOfT]
        where
            dSumCon = mkTyCon3 "dependent-sum" "Data.Dependent.Sum" "DSum"
            typeOfT = typeOf1 $ (undefined :: DSum f -> f a) ds

#else 

instance Typeable1 t => Typeable (DSum t) where
    typeOf ds = mkTyConApp dSumCon [typeOfT]
        where
            dSumCon = mkTyCon "Data.Dependent.Sum.DSum"
            typeOfT = typeOf1 $ (undefined :: DSum f -> f a) ds

#endif