{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Data.Some where

import Data.GADT.Show
import Data.GADT.Compare
import Data.Maybe

data Some tag where
    This :: !(tag t) -> Some tag

withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (This thing) some = some thing

instance GShow tag => Show (Some tag) where
    showsPrec p (This thing) = showParen (p > 10)
        ( showString "This "
        . gshowsPrec 11 thing
        )

instance GRead tag => Read (Some tag) where
    readsPrec p = readParen (p > 1) $ \s -> 
        [ withTag $ \tag -> (This tag, rest')
        | let (con, rest) = splitAt 5 s
        , con == "This "
        , (withTag, rest') <- greadsPrec p rest -- TODO: check precedence
        ]

instance GEq tag => Eq (Some tag) where
    This x == This y = defaultEq x y

instance GCompare tag => Ord (Some tag) where
    compare (This x) (This y) = defaultCompare x y
