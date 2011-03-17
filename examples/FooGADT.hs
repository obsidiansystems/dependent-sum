{-# LANGUAGE GADTs #-}
module FooGADT where

import Data.Dependent.Sum
import Data.GADT.Show
import Data.GADT.Compare

data Foo a where
    Foo :: Foo Double
    Bar :: Foo Int
    Baz :: Foo String
    Qux :: Foo Double

instance Eq (Foo a) where
    Foo == Foo  = True
    Bar == Bar  = True
    Baz == Baz  = True
    Qux == Qux  = True
    _   == _    = False

instance GEq Foo where
    geq Foo Foo = Just Refl
    geq Bar Bar = Just Refl
    geq Baz Baz = Just Refl
    geq Qux Qux = Just Refl
    geq _   _   = Nothing
    
    maybeEq Foo Foo y n = y
    maybeEq Bar Bar y n = y
    maybeEq Baz Baz y n = y
    maybeEq Qux Qux y n = y
    maybeEq _   _   y n = n
    

instance EqTag Foo where
    eqTagged Foo Foo = (==)
    eqTagged Bar Bar = (==)
    eqTagged Baz Baz = (==)
    eqTagged Qux Qux = (==)
    eqTagged _   _   = const (const False)

instance GCompare Foo where
    gcompare Foo Foo = GEQ
    gcompare Foo _   = GLT
    
    gcompare Bar Foo = GGT
    gcompare Bar Bar = GEQ
    gcompare Bar _   = GLT
    
    gcompare Baz Foo = GGT
    gcompare Baz Bar = GGT
    gcompare Baz Baz = GEQ
    gcompare Baz _   = GLT
    
    gcompare Qux Foo = GGT
    gcompare Qux Bar = GGT
    gcompare Qux Baz = GGT
    gcompare Qux Qux = GEQ

instance OrdTag Foo where
    compareTagged Foo Foo = compare
    compareTagged Bar Bar = compare
    compareTagged Baz Baz = compare
    compareTagged Qux Qux = compare
    
    compareTagged _   _   = error "OrdTag: bad case"

instance Show (Foo a) where
    showsPrec _ Foo      = showString "Foo"
    showsPrec _ Bar      = showString "Bar"
    showsPrec _ Baz      = showString "Baz"
    showsPrec _ Qux      = showString "Qux"

instance GShow Foo where
    gshowsPrec = showsPrec

instance ShowTag Foo where
    showTaggedPrec Foo = showsPrec
    showTaggedPrec Bar = showsPrec
    showTaggedPrec Baz = showsPrec
    showTaggedPrec Qux = showsPrec


instance GRead Foo where
    greadsPrec _ str = case tag of
        "Foo" -> [(\k -> k Foo, rest)]
        "Bar" -> [(\k -> k Bar, rest)]
        "Baz" -> [(\k -> k Baz, rest)]
        "Qux" -> [(\k -> k Qux, rest)]
        _     -> []
        where (tag, rest) = splitAt 3 str

instance ReadTag Foo where
    readTaggedPrec Foo = readsPrec
    readTaggedPrec Bar = readsPrec
    readTaggedPrec Baz = readsPrec
    readTaggedPrec Qux = readsPrec


foo x = Foo :=> x
bar x = Bar :=> x
baz x = Baz :=> x
qux x = Qux :=> x


xs = [foo pi, bar 100, baz "hello world", qux (exp 1)]
xs' = read (show xs) `asTypeOf` xs