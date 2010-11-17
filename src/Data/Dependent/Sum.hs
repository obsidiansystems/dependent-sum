{-# LANGUAGE ExistentialQuantification, GADTs #-}
module Data.Dependent.Sum where

import Data.GADT.Show
import Data.GADT.Compare

import Data.Maybe (fromMaybe)

-- |A basic dependent sum type; the first component is a tag that specifies 
-- the type of the second;  for example, think of a GADT such as:
-- 
-- > data Tag a where
-- >    AString :: Tag String
-- >    AnInt   :: Tag Int
-- 
-- Then, we have the following valid expressions of type @DSum Tag@:
--
-- > AString :=> "hello!"
-- > AnInt   :=> 42
-- 
-- And we can right functions that consume @DSum Tag@ by matching, such as:
-- 
-- > toString :: DSum Tag -> String
-- > toString (AString :=> str) = str
-- > toString (AnInt   :=> int) = show int
-- 
-- By analogy to the (key => value) construction for dictionary entries in 
-- many dynamic languages, we use (key :=> value) as the constructor for 
-- dependent sums.  The :=> operator has very low precedence and binds to 
-- the right, so if the @Tag@ GADT is extended with an additional constructor
-- @Rec :: Tag (DSum Tag)@, then @Rec :=> AnInt :=> 3 + 4@ is parsed as
-- would be expected and has type @DSum Tag@.  Its precedence is just above
-- that of '$', so @foo bar $ AString :=> "eep"@ is equivalent to
-- @foo bar (AString :=> "eep")@.
data DSum tag = forall a. tag a :=> a
infixr 1 :=>

-- |In order to make a 'Show' instance for @DSum tag@, @tag@ must be able
-- to show itself as well as any value of the tagged type.  'GShow' together
-- with this class provides the interface by which it can do so.
--
-- The @Tag@ example type introduced in the 'DSum' section could be given the
-- following instances:
-- 
-- > instance GShow Tag where
-- >     gshowsPrec _showsValPrec _p AString = showString "AString"
-- >     gshowsPrec _showsValPrec _p AnInt   = showString "AnInt"
-- > instance ShowTag Tag where
-- >     showTaggedPrec AString = showsPrec
-- >     showTaggedPrec AnInt   = showsPrec
-- 
class GShow tag => ShowTag tag where
    showTaggedPrec :: tag a -> Int ->     a -> ShowS

instance ShowTag tag => Show (DSum tag) where
    showsPrec p (tag :=> value) = showParen (p >= 10)
        ( gshowsPrec (showTaggedPrec tag) 0 tag
        . showString " :=> "
        . showTaggedPrec tag 1 value
        )

-- |In order to test @DSum tag@ for equality, 'tag' must know how to test
-- both itself and its tagged values for equality.  'GCompare' and 'EqTag'
-- together provide the interface by which they are expected to do so.
-- 
-- Continuing the @Tag@ example from the 'DSum' section, we can define:
-- 
-- instance GCompare Tag where
--     gcompare AString AString = GEQ
--     gcompare AString AnInt   = GLT
--     gcompare AnInt   AString = GGT
--     gcompare AnInt   AnInt   = GEQ
-- instance EqTag Tag where
--     eqTagged AString AString = (==)
--     eqTagged AnInt   AnInt   = (==)
class GCompare tag => EqTag tag where
    eqTagged :: tag a -> tag a -> a -> a -> Bool

instance EqTag tag => Eq (DSum tag) where
    (t1 :=> x1) == (t2 :=> x2)  = fromMaybe False $ do
        GEQ <- geq t1 t2
        return (eqTagged t1 t2 x1 x2)

-- |In order to compare @DSum tag@ values, 'tag' must know how to compare
-- both itself and its tagged values.  'GCompare', 'EqTag' and 'OrdTag'
-- together provide the interface by which they are expected to do so.
-- 
-- Continuing the @Tag@ example from the 'EqTag' section, we can define:
-- 
-- instance OrdTag Tag where
--     ordTagged AString AString = compare
--     ordTagged AnInt   AnInt   = compare
class EqTag tag => OrdTag tag where
    ordTagged :: tag a -> tag a -> a -> a -> Ordering

instance OrdTag tag => Ord (DSum tag) where
    compare (t1 :=> x1) (t2 :=> x2)  = case gcompare t1 t2 of
        GLT -> LT
        GGT -> GT
        GEQ -> ordTagged t1 t2 x1 x2