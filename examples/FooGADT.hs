{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FooGADT where

import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Show
import Data.GADT.Compare

data Foo a where
    Foo :: Foo Double
    Bar :: Foo Int
    Baz :: Foo String
    Qux :: Foo Double

instance Eq (Foo a) where
    (==) = defaultEq

instance GEq Foo where
    geq Foo Foo = Just Refl
    geq Bar Bar = Just Refl
    geq Baz Baz = Just Refl
    geq Qux Qux = Just Refl
    geq _   _   = Nothing

instance EqTag Foo Identity where
    eqTagged Foo Foo = (==)
    eqTagged Bar Bar = (==)
    eqTagged Baz Baz = (==)
    eqTagged Qux Qux = (==)
    eqTagged _   _   = const (const False)

instance GCompare Foo where
    gcompare Foo Foo = GEQ
    gcompare Foo _   = GLT
    gcompare _   Foo = GGT
    
    gcompare Bar Bar = GEQ
    gcompare Bar _   = GLT
    gcompare _   Bar = GGT
    
    gcompare Baz Baz = GEQ
    gcompare Baz _   = GLT
    gcompare _   Baz = GGT
    
    gcompare Qux Qux = GEQ

instance OrdTag Foo Identity where
    compareTagged Foo Foo = compare
    compareTagged Bar Bar = compare
    compareTagged Baz Baz = compare
    compareTagged Qux Qux = compare
    
    compareTagged _   _   = error "OrdTag (Foo): bad case"

instance Show (Foo a) where
    showsPrec _ Foo      = showString "Foo"
    showsPrec _ Bar      = showString "Bar"
    showsPrec _ Baz      = showString "Baz"
    showsPrec _ Qux      = showString "Qux"

instance GShow Foo where
    gshowsPrec = showsPrec

instance ShowTag Foo Identity where
    showTaggedPrec Foo = showsPrec
    showTaggedPrec Bar = showsPrec
    showTaggedPrec Baz = showsPrec
    showTaggedPrec Qux = showsPrec


instance GRead Foo where
    greadsPrec _ str = case tag of
        "Foo" -> [(GReadResult (\k -> k Foo), rest)]
        "Bar" -> [(GReadResult (\k -> k Bar), rest)]
        "Baz" -> [(GReadResult (\k -> k Baz), rest)]
        "Qux" -> [(GReadResult (\k -> k Qux), rest)]
        _     -> []
        where (tag, rest) = splitAt 3 str

instance ReadTag Foo Identity where
    readTaggedPrec Foo = readsPrec
    readTaggedPrec Bar = readsPrec
    readTaggedPrec Baz = readsPrec
    readTaggedPrec Qux = readsPrec

foo :: Double -> DSum Foo Identity
foo x = Foo ==> x

bar :: Int -> DSum Foo Identity
bar x = Bar ==> x

baz :: String -> DSum Foo Identity
baz x = Baz ==> x

qux :: Double -> DSum Foo Identity
qux x = Qux ==> x

xs = [foo pi, bar 100, baz "hello world", qux (exp 1)]
xs' = read (show xs) `asTypeOf` xs