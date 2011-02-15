{-# LANGUAGE GADTs#-}
module Data.GADT.Compare where

import Control.Monad (MonadPlus(..))

-- |A type for the result of comparing GADT constructors; the type parameters
-- of the GADT values being compared are included so that in the case where 
-- they are equal their parameter types can be unified.
data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b

-- |Type class for comparable GADT-like structures.  When 2 things are equal,
-- must return a witness that their parameter types are equal as well ('GEQ').
class GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

-- |Convenient function for simply obtaining a witness of type-equality, if
-- one exists.  It will never return a constructor other than 'GEQ', so it 
-- is safe to pattern-bind against only that case.
-- 
-- A handy idiom for using this would be to pattern-bind in the Maybe monad, eg.:
-- 
-- > extract :: GCompare tag => tag a -> DSum tag -> Maybe a
-- > extract t1 (t2 :=> x) = do
-- >     GEQ <- geq t1 t2
-- >     return x
-- 
-- Or in a list comprehension:
-- 
-- > extractMany :: GCompare tag => tag a -> [DSum tag] -> [a]
-- > extractMany t1 things = [ x | (t2 :=> x) <- things, GEQ <- geq t1 t2]
--
-- (Making use of the 'DSum' type from "Data.Dependent.Sum" in both examples)
geq :: (GCompare f, MonadPlus m) => f a -> f b -> m (GOrdering a b)
geq a b = case gcompare a b of
    GEQ -> return GEQ
    _   -> mzero

