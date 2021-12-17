{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Data.GADT.Compare.Monad
    ( GComparing
    , runGComparing
    , geq'
    , compare'
    ) where

import Control.Applicative
import Control.Monad
import Data.GADT.Compare
import Data.Type.Equality ((:~:) (..))

-- A monad allowing gcompare to be defined in the same style as geq
newtype GComparing a b t = GComparing (Either (GOrdering a b) t)

instance Functor (GComparing a b) where fmap f (GComparing x) = GComparing (either Left (Right . f) x)
instance Monad (GComparing a b) where
    return = pure
    GComparing (Left  x) >>= f = GComparing (Left x)
    GComparing (Right x) >>= f = f x
instance Applicative (GComparing a b) where
    pure = GComparing . Right
    (<*>) = ap

geq' :: GCompare t => t a -> t b -> GComparing x y (a :~: b)
geq' x y = GComparing (case gcompare x y of
    GLT -> Left GLT
    GEQ -> Right Refl
    GGT -> Left GGT)

compare' x y = GComparing $ case compare x y of
    LT -> Left GLT
    EQ -> Right ()
    GT -> Left GGT

runGComparing (GComparing x) = either id id x
