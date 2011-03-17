{-# LANGUAGE GADTs, TypeOperators, RankNTypes, TypeFamilies #-}
module Data.GADT.Compare where

data a := b where
    Refl :: a := a

instance Eq (a := b) where
    Refl == Refl = True

instance Ord (a := b) where
    compare Refl Refl = EQ

instance Show (a := b) where
    showsPrec _ Refl = showString "Refl"

-- |A class for type-contexts which contain enough information
-- to (at least in some cases) decide the equality of types 
-- occurring within them.
-- 
-- Minimal instance declaration is either 'geq' or 'maybeEq'.
class GEq f where
    -- |Produce a witness of type-equality, if one exists.
    -- 
    -- A handy idiom for using this would be to pattern-bind in the Maybe monad, eg.:
    -- 
    -- > extract :: GEq tag => tag a -> DSum tag -> Maybe a
    -- > extract t1 (t2 :=> x) = do
    -- >     Refl <- geq t1 t2
    -- >     return x
    -- 
    -- Or in a list comprehension:
    -- 
    -- > extractMany :: GEq tag => tag a -> [DSum tag] -> [a]
    -- > extractMany t1 things = [ x | (t2 :=> x) <- things, Refl <- maybeToList (geq t1 t2)]
    --
    -- (Making use of the 'DSum' type from "Data.Dependent.Sum" in both examples)
    geq :: f a -> f b -> Maybe (a := b)
    geq = maybeEq (Just Refl) Nothing
    
    -- An interesting alternative formulation:
    -- This one is nice because it's purely type-level, which means
    -- that in some cases the type checker can statically prove
    -- that the 'f' case is unreachable.
    -- 
    -- Sometimes, though, it can be hard to get the 'Refl' case's type to unify
    -- with the assumptions properly.
    maybeEq :: ((a ~ b) => c) -> c -> f a -> f b -> c
    maybeEq f z x y = case geq x y of
        Just Refl   -> f
        Nothing     -> z

instance GEq ((:=) a) where
    geq Refl Refl = Just Refl

instance GEq (GOrdering a) where
    geq GEQ GEQ = Just Refl
    geq   _   _ = Nothing

-- This instance seems nice, but it's simply not right:
-- 
-- > instance GEq StableName where
-- >     geq sn1 sn2
-- >         | sn1 == unsafeCoerce sn2
-- >             = Just (unsafeCoerce Refl)
-- >         | otherwise     = Nothing
-- 
-- Proof:
-- 
-- > x <- makeStableName id :: IO (StableName (Int -> Int))
-- > y <- makeStableName id :: IO (StableName ((Int -> Int) -> Int -> Int))
-- > 
-- > let Just boom = geq x y
-- > let coerce :: (a := b) -> a -> b; coerce Refl = id
-- > 
-- > coerce boom (const 0) id 0
-- > let "Illegal Instruction" = "QED."
-- 
-- The core of the problem is that 'makeStableName' only knows the closure
-- it is passed to, not any type information.  Together with the fact that
-- the same closure has the same StableName each time 'makeStableName' is 
-- called on it, there is serious potential for abuse when a closure can 
-- be given many incompatible types.


-- |A type for the result of comparing GADT constructors; the type parameters
-- of the GADT values being compared are included so that in the case where 
-- they are equal their parameter types can be unified.
data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b

-- |TODO: Think of a better name
--
-- This operation forgets the phantom types of a 'GOrdering' value.
weakenOrdering :: GOrdering a b -> Ordering
weakenOrdering GLT = LT
weakenOrdering GEQ = EQ
weakenOrdering GGT = GT

instance Eq (GOrdering a b) where
    x == y =
        weakenOrdering x == weakenOrdering y

instance Ord (GOrdering a b) where
    compare x y = compare (weakenOrdering x) (weakenOrdering y)

instance Show (GOrdering a b) where
    showsPrec _ GGT = showString "GGT"
    showsPrec _ GEQ = showString "GEQ"
    showsPrec _ GLT = showString "GLT"

-- |Type class for orderable GADT-like structures.  When 2 things are equal,
-- must return a witness that their parameter types are equal as well (GEQ).
-- |Type class for comparable GADT-like structures.  When 2 things are equal,
-- must return a witness that their parameter types are equal as well ('GEQ').
class GEq f => GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

instance GCompare ((:=) a) where
    gcompare Refl Refl = GEQ

