{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH

data MySum :: * -> * where
  MySum_Int :: MySum Int
  MySum_String :: MySum String

deriving instance Show (MySum a)

deriveGShow ''MySum
deriveGEq ''MySum
deriveGCompare ''MySum

main :: IO ()
main = do
  guard $ show MySum_Int == gshow MySum_Int
  guard $ show MySum_String == gshow MySum_String
  guard $ (MySum_Int `geq` MySum_Int) == Just Refl
  guard $ (MySum_Int `gcompare` MySum_Int) == GEQ
  guard $ (MySum_String `geq` MySum_String) == Just Refl
  guard $ (MySum_String `gcompare` MySum_String) == GEQ
  guard $ (MySum_Int `gcompare` MySum_String) == GLT
  guard $ (MySum_String `gcompare` MySum_Int) == GGT
  return ()

--TODO: Figure out how to best use these test cases; just checking that they
-- compile is useful, but it's probably more useful to check some properties as
-- well

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

instance Show (Foo a) where showsPrec = gshowsPrec
instance Show (Bar a) where showsPrec = gshowsPrec
instance Show (Baz a) where showsPrec = gshowsPrec
instance Show (Qux a) where showsPrec = gshowsPrec

deriveGShow ''Foo
deriveGShow ''Bar
deriveGShow ''Baz
deriveGShow ''Qux

data Squudge a where
    E :: Ord a => Foo a -> Squudge a

deriveGEq ''Squudge
deriveGCompare ''Squudge
deriveGShow ''Squudge
instance Show (Squudge a) where showsPrec = gshowsPrec

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

deriving instance Show a => Show (Splort a)

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
    [geqInst, gcompareInst, gshowInst] <-
        [d|
            instance GEq a => GEq (Spleeb a)
            instance GCompare a => GCompare (Spleeb a)
            instance Show (a Double) => GShow (Spleeb a)
          |]

    concat <$> sequence
        [ deriveGEq      geqInst
        , deriveGCompare gcompareInst
        , deriveGShow    gshowInst
        ]

instance Show (a Double) => Show (Spleeb a b) where showsPrec = gshowsPrec

-- another option; start from the declaration and juggle that a bit
do
    decs <- [d|
        data Fnord a where Yarr :: Fnord Double; Grr :: Fnord (Int -> String)
     |]

    geqInst         <- deriveGEq      decs
    gcompareInst    <- deriveGCompare decs
    gshowInst       <- deriveGShow    decs

    return $ concat
        [ decs
        , geqInst
        , gcompareInst
        , gshowInst
        ]

instance Show (Fnord a) where showsPrec = gshowsPrec
