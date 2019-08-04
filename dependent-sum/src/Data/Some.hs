{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
module Data.Some (Some(Some, This), mkSome, withSome, mapSome) where

import Data.GADT.Show
import Data.GADT.Compare
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- $setup
-- >>> :set -XKindSignatures -XGADTs

-- | An existential wrapper useful for hiding GADTs' parameters.
--
-- @Some@ could conceptually be defined
--
-- @
-- data Some f where
--   Some :: forall a. !(f a) -> Some f
-- @
--
-- Unlike that conceptual definition, there is no extra indirection from
-- a data constructor. The @Some@ pattern synonym simulates this definition.
--
-- === Example
--
-- >>> data Tag :: * -> * where TagInt :: Tag Int; TagBool :: Tag Bool
-- >>> instance GShow Tag where gshowsPrec _ TagInt = showString "TagInt"; gshowsPrec _ TagBool = showString "TagBool"
--
-- Using the pattern synonym (recommended):
--
-- >>> let x = Some TagInt
-- >>> x
-- Some TagInt
--
-- >>> case x of { Some TagInt -> "I"; Some TagBool -> "B" } :: String
-- "I"
--
-- Alternatively, you can use explicit packing and unpacking functions:
--
-- >>> let y = mkSome TagBool
-- >>> y
-- Some TagBool
--
-- >>> withSome y $ \y' -> case y' of { TagInt -> "I"; TagBool -> "B" } :: String
-- "B"
--
-- The implementation of 'mapSome' is /safe/.
--
-- >>> let f :: Tag a -> Tag a; f TagInt = TagInt; f TagBool = TagBool
-- >>> mapSome f y
-- Some TagBool
--
-- but you can also use:
--
-- >>> withSome y (mkSome . f)
-- Some TagBool
--
newtype Some tag = UnsafeSome (tag Any)
data SBox a = SBox !a

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE Some #-}
#endif
pattern Some :: tag a -> Some tag
pattern Some x <- UnsafeSome (SBox . (unsafeCoerce :: tag Any -> tag a) -> SBox x)
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE This #-}
#endif
{-# DEPRECATED This "Use 'Some' instead" #-}
pattern This :: tag a -> Some tag
pattern This x = Some x

-- | Constructor.
mkSome :: tag a -> Some tag
mkSome = Some

-- | Eliminator.
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

mapSome :: (forall t. f t -> g t) -> Some f -> Some g
mapSome f (UnsafeSome x) = UnsafeSome (f x)
