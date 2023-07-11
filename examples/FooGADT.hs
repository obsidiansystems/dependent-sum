{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module FooGADT where

import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Show
import Data.GADT.Compare
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.List (sort)

data Foo a where
    Foo :: Foo Double
    Bar :: Foo Int
    Baz :: Foo String
    Qux :: Foo Double

deriveArgDict ''Foo

{-
-- NB: The instance for ArgDict could be manually written as:

instance ArgDict Foo where
    type ConstraintsFor Foo c = (c Double, c Int, c String)
    argDict x = case x of
        Foo -> Dict
        Bar -> Dict
        Baz -> Dict
        Qux -> Dict
-}

instance Eq (Foo a) where
    (==) = defaultEq

instance GEq Foo where
    geq Foo Foo = Just Refl
    geq Bar Bar = Just Refl
    geq Baz Baz = Just Refl
    geq Qux Qux = Just Refl
    geq _   _   = Nothing

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

instance Show (Foo a) where
    showsPrec _ Foo      = showString "Foo"
    showsPrec _ Bar      = showString "Bar"
    showsPrec _ Baz      = showString "Baz"
    showsPrec _ Qux      = showString "Qux"

instance GShow Foo where
    gshowsPrec = showsPrec

instance GRead Foo where
    greadsPrec _ str = case tag of
        "Foo" -> [(GReadResult (\k -> k Foo), rest)]
        "Bar" -> [(GReadResult (\k -> k Bar), rest)]
        "Baz" -> [(GReadResult (\k -> k Baz), rest)]
        "Qux" -> [(GReadResult (\k -> k Qux), rest)]
        _     -> []
        where (tag, rest) = splitAt 3 str

foo :: Double -> DSum Foo Identity
foo x = Foo ==> x

bar :: Int -> DSum Foo Identity
bar x = Bar ==> x

baz :: String -> DSum Foo Identity
baz x = Baz ==> x

qux :: Double -> DSum Foo Identity
qux x = Qux ==> x

xs, xs', xs'' :: [DSum Foo Identity]
xs = [bar 100, foo pi, qux (exp 1), baz "hello world"]
xs' = read (show xs) `asTypeOf` xs
xs'' = sort xs