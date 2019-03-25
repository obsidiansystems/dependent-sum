{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Some (Some(This), withSome) where

import Data.GADT.Show
import Data.GADT.Compare
import Data.Maybe
import GHC.Exts (Any)
import Unsafe.Coerce

newtype Some tag = UnsafeSome (tag Any)

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE This #-}
#endif
pattern This :: tag a -> Some tag
pattern This x <- UnsafeSome ((unsafeCoerce :: tag Any -> tag a) -> x)
  where This x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)

withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (This thing) some = some thing

instance GShow tag => Show (Some tag) where
    showsPrec p (This thing) = showParen (p > 10)
        ( showString "This "
        . gshowsPrec 11 thing
        )

instance GRead f => Read (Some f) where
    readsPrec p = readParen (p>10) $ \s ->
        [ (getGReadResult withTag This, rest')
        | let (con, rest) = splitAt 5 s
        , con == "This "
        , (withTag, rest') <- greadsPrec 11 rest
        ]

instance GEq tag => Eq (Some tag) where
    This x == This y = defaultEq x y

instance GCompare tag => Ord (Some tag) where
    compare (This x) (This y) = defaultCompare x y
