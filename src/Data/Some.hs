{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Some (Some(Some, This), withSome) where

import Data.GADT.Show
import Data.GADT.Compare
import Data.Maybe
import GHC.Exts (Any)
import Unsafe.Coerce

newtype Some tag = UnsafeSome (tag Any)

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE Some #-}
#endif
pattern Some :: tag a -> Some tag
pattern Some x <- UnsafeSome ((unsafeCoerce :: tag Any -> tag a) -> x)
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE This #-}
#endif
{-# DEPRECATED This "Use 'Some' instead" #-}
pattern This :: tag a -> Some tag
pattern This x = Some x

withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (Some thing) some = some thing

instance GShow tag => Show (Some tag) where
    showsPrec p (Some thing) = showParen (p > 10)
        ( showString "Some "
        . gshowsPrec 11 thing
        )

instance GRead f => Read (Some f) where
    readsPrec p = readParen (p>10) $ \s ->
        [ (getGReadResult withTag Some, rest')
        | let (con, rest) = splitAt 5 s
        , con == "Some "
        , (withTag, rest') <- greadsPrec 11 rest
        ]

instance GEq tag => Eq (Some tag) where
    Some x == Some y = defaultEq x y

instance GCompare tag => Ord (Some tag) where
    compare (Some x) (Some y) = defaultCompare x y
