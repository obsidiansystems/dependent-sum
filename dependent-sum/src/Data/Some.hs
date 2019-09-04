{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Some
  ( Some (Some, This)
  , ThenSome (..)
  , BeforeSome (..)
  , mkSome
  , withSome
  , mapSome
  ) where

import Data.GADT.Show
import Data.GADT.Compare
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Coerce (coerce)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup ((<>)))
#endif

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
pattern Some x <- UnsafeSome ((unsafeCoerce :: tag Any -> tag a) -> x)
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

-- | A 'Monoid' using '*>' for an underlying 'Applicative' functor.
newtype ThenSome f = ThenSome { getThenSome :: Some f }
  deriving (Eq, Ord, Read, Show)

#if MIN_VERSION_base(4,9,0)
-- This should really be Apply, but we can't do that for now.
instance Applicative f => Semigroup (ThenSome f) where
    (<>) = coerce ((*>) :: f Any -> f Any -> f Any)
#endif

instance Applicative f => Monoid (ThenSome f) where
  mempty = ThenSome (Some (pure ()))
#if !MIN_VERSION_base(4,11,0)
  mappend = coerce ((*>) :: f Any -> f Any -> f Any)
#endif

-- | A 'Monoid' using '<*' for an underlying 'Applicative' functor.
newtype BeforeSome f = BeforeSome { getBeforeSome :: Some f }
  deriving (Eq, Ord, Read, Show)

#if MIN_VERSION_base(4,9,0)
-- This should really be Apply, but we can't do that for now.
instance Applicative f => Semigroup (BeforeSome f) where
  (<>) = coerce ((<*) :: f Any -> f Any -> f Any)
#endif

instance Applicative f => Monoid (BeforeSome f) where
  mempty = BeforeSome (Some (pure ()))
#if !MIN_VERSION_base(4,11,0)
  mappend = coerce ((<*) :: f Any -> f Any -> f Any)
#endif
