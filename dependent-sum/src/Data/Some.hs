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

-- | Existential. This is type is useful to hide GADTs' parameters.
--
-- >>> data Tag :: * -> * where TagInt :: Tag Int; TagBool :: Tag Bool
-- >>> instance GShow Tag where gshowsPrec _ TagInt = showString "TagInt"; gshowsPrec _ TagBool = showString "TagBool"
--
-- You can either use @PatternSynonyms@
--
-- >>> let x = Some TagInt
-- >>> x
-- Some TagInt
--
-- >>> case x of { Some TagInt -> "I"; Some TagBool -> "B" } :: String
-- "I"
--
-- or you can use functions
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

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE Some #-}
#endif
pattern Some :: tag a -> Some tag
#if __GLASGOW_HASKELL__ >= 802
pattern Some x <- UnsafeSome x
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)
#else
-- There was a bug type checking pattern synonyms that prevented the
-- obvious thing from working.
pattern Some x <- UnsafeSome ((unsafeCoerce :: tag Any -> tag a) -> x)
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)
#endif


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
