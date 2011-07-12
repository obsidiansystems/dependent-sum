{-# LANGUAGE ExistentialQuantification, GADTs #-}
module Data.Dependent.Sum where

import Data.GADT.Show
import Data.GADT.Compare
import Data.Typeable

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
-- And we can write functions that consume @DSum Tag@ values by matching, 
-- such as:
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
-- would be expected (@Rec :=> (AnInt :=> (3 + 4))@) and has type @DSum Tag@.
-- Its precedence is just above that of '$', so @foo bar $ AString :=> "eep"@
-- is equivalent to @foo bar (AString :=> "eep")@.
data DSum tag = forall a. !(tag a) :=> a
infixr 1 :=>

instance Typeable1 t => Typeable (DSum t) where
    typeOf ds = mkTyConApp dSumCon [typeOfT]
        where
            dSumCon = mkTyCon "Data.Dependent.Sum.DSum"
            typeOfT = typeOf1 $ (undefined :: DSum f -> f a) ds

-- |In order to make a 'Show' instance for @DSum tag@, @tag@ must be able
-- to show itself as well as any value of the tagged type.  'GShow' together
-- with this class provides the interface by which it can do so.
--
-- @GShow tag => t@ is conceptually equivalent to something like this
-- imaginary syntax:  @(forall a. Inhabited (tag a) => Show a) => t@,
-- where 'Inhabited' is an imaginary predicate that characterizes 
-- non-empty types, and 'a' does not occur free in 't'.
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
    -- |Given a value of type @tag a@, return the 'showsPrec' function for 
    -- the type parameter @a@.
    showTaggedPrec :: tag a -> Int ->     a -> ShowS

instance Show a => ShowTag ((:=) a) where
    showTaggedPrec Refl = showsPrec

-- This instance is questionable.  It works, but is pretty useless.
instance Show a => ShowTag (GOrdering a) where
    showTaggedPrec GEQ = showsPrec
    showTaggedPrec _   = \p _ -> showParen (p > 10)
        ( showString "error "
        . shows "type information lost into the mists of oblivion"
        )

instance ShowTag tag => Show (DSum tag) where
    showsPrec p (tag :=> value) = showParen (p >= 10)
        ( gshowsPrec 0 tag
        . showString " :=> "
        . showTaggedPrec tag 1 value
        )

class GRead tag => ReadTag tag where
    readTaggedPrec :: tag a -> Int -> ReadS a

instance Read a => ReadTag ((:=) a) where
    readTaggedPrec Refl = readsPrec

-- This instance is questionable.  It works, but is partial (and is also pretty useless)
-- instance Read a => ReadTag (GOrdering a) where
--     readTaggedPrec GEQ = readsPrec
--     readTaggedPrec tag = \p -> readParen (p>10) $ \s ->
--         [ (error msg, rest')
--         | let (con, rest) = splitAt 6 s
--         , con == "error "
--         , (msg, rest') <- reads rest :: [(String, String)]
--         ]

instance ReadTag tag => Read (DSum tag) where
    readsPrec p = readParen (p > 1) $ \s -> 
        concat
            [ withTag $ \tag ->
                [ (tag :=> val, rest'')
                | (val, rest'') <- readTaggedPrec tag 1 rest'
                ]
            | (withTag, rest) <- greadsPrec p s
            , let (con, rest') = splitAt 5 rest
            , con == " :=> "
            ]

-- |In order to test @DSum tag@ for equality, @tag@ must know how to test
-- both itself and its tagged values for equality.  'EqTag' defines
-- the interface by which they are expected to do so.
-- 
-- Continuing the @Tag@ example from the 'DSum' section, we can define:
-- 
-- > instance GEq Tag where
-- >     geq AString AString = Just Refl
-- >     geq AString AnInt   = Nothing
-- >     geq AnInt   AString = Nothing
-- >     geq AnInt   AnInt   = Just Refl
-- > instance EqTag Tag where
-- >     eqTagged AString AString = (==)
-- >     eqTagged AnInt   AnInt   = (==)
-- 
-- Note that 'eqTagged' is not called until after the tags have been
-- compared, so it only needs to consider the cases where 'gcompare' returns 'GEQ'.
class GEq tag => EqTag tag where
    -- |Given two values of type @tag a@ (for which 'gcompare' returns 'GEQ'),
    -- return the '==' function for the type @a@.
    eqTagged :: tag a -> tag a -> a -> a -> Bool

instance Eq a => EqTag ((:=) a) where
    eqTagged Refl Refl = (==)

instance EqTag tag => Eq (DSum tag) where
    (t1 :=> x1) == (t2 :=> x2)  = fromMaybe False $ do
        Refl <- geq t1 t2
        return (eqTagged t1 t2 x1 x2)

-- |In order to compare @DSum tag@ values, @tag@ must know how to compare
-- both itself and its tagged values.  'OrdTag' defines the 
-- interface by which they are expected to do so.
-- 
-- Continuing the @Tag@ example from the 'EqTag' section, we can define:
-- 
-- > instance GCompare Tag where
-- >     gcompare AString AString = GEQ
-- >     gcompare AString AnInt   = GLT
-- >     gcompare AnInt   AString = GGT
-- >     gcompare AnInt   AnInt   = GEQ
-- > instance OrdTag Tag where
-- >     compareTagged AString AString = compare
-- >     compareTagged AnInt   AnInt   = compare
-- 
-- As with 'eqTagged', 'compareTagged' only needs to consider cases where
-- 'gcompare' returns 'GEQ'.
class (EqTag tag, GCompare tag) => OrdTag tag where
    -- |Given two values of type @tag a@ (for which 'gcompare' returns 'GEQ'),
    -- return the 'compare' function for the type @a@.
    compareTagged :: tag a -> tag a -> a -> a -> Ordering

instance Ord a => OrdTag ((:=) a) where
    compareTagged Refl Refl = compare

instance OrdTag tag => Ord (DSum tag) where
    compare (t1 :=> x1) (t2 :=> x2)  = case gcompare t1 t2 of
        GLT -> LT
        GGT -> GT
        GEQ -> compareTagged t1 t2 x1 x2