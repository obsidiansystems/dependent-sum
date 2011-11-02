{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Test where

import Language.Haskell.TH
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Control.Monad

-- test cases: should be able to generate instances for these
-- (Bar requiring the existence of an instance for Foo)
data Foo a where
    I :: Foo Int
    D :: Foo Double
    A :: Foo a -> Foo b -> Foo (a -> b)

data Bar a where
    F :: Foo a -> Bar a
    S :: Bar String

data Baz a where
    L :: Qux a -> Int -> Baz [a]

data Qux a where
    FB :: Foo (a -> b) -> Bar b -> Qux (a -> (b, b))

deriveGEq ''Foo
deriveGEq ''Bar
deriveGEq ''Baz
deriveGEq ''Qux

deriveGCompare ''Foo
deriveGCompare ''Bar
deriveGCompare ''Baz
deriveGCompare ''Qux

data Squudge a where
    E :: Ord a => Foo a -> Squudge a

deriveGEq ''Squudge
deriveGCompare ''Squudge

data Splort a where
    Splort :: Squudge a -> a -> Splort a

-- -- deriveGEq ''Splort
-- This one theoretically could work (instance explicitly given below), but I don't think
-- it's something I want to try to automagically support.  It would require actually
-- matching on sub-constructors, which could get pretty ugly, especially since it may
-- not even be the case that a finite number of matches would suffice.
instance GEq Splort where
    geq (Splort (E x1) x2) (Splort (E y1) y2) = do
        Refl <- geq x1 y1
        guard (x2 == y2)
        Just Refl

instance GCompare Splort where
    gcompare (Splort (E x1) x2) (Splort (E y1) y2) = 
        runGComparing $ do
            Refl <- geq' x1 y1
            compare' x2 y2
            return GEQ

-- Also should work for empty types
data Empty a
deriveGEq ''Empty
deriveGCompare ''Empty

-- Also supports types with multiple parameters, by quoting empty instance declarations
-- ([t||] brackets won't work because they can only quote types of kind *).
data Spleeb a b where
    P :: a Double -> Qux b -> Spleeb a b
-- need a cleaner 'one-shot' way of defining these - the empty instances need to appear 
-- in the same quotation because the GEq context of the GCompare class causes stage
-- restriction errors... seems like GHC shouldn't actually check things like that till
-- the final splice, but whatever.
do
    [geqInst, gcompareInst] <- 
        [d|
            instance GEq a => GEq (Spleeb a)
            instance GCompare a => GCompare (Spleeb a)
          |]
    
    liftM2 (++) (deriveGEq geqInst) (deriveGCompare gcompareInst)
