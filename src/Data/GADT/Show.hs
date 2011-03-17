{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Data.GADT.Show where

-- |'Show'-like class for 1-type-parameter GADTs; I'm not entirely sure
-- whether the show function should be given the means to show the parameter 
-- type or should be expected to know how to do so already.  For now, one will
-- be provided.
class GShow t where
    -- |Given the 'showsPrec' function of the type parameter @a@, return a
    -- 'showsPrec' function for the type @t a@.
    gshowsPrec :: (Int -> a -> ShowS) -> Int -> t a -> ShowS


gshows :: (GShow t, Show a) => t a -> ShowS
gshows = gshowsPrec showsPrec (-1)

gshow :: (GShow t, Show a) => t a -> String
gshow x = gshows x ""


type GReadS t = String -> [(forall b. (forall a. t a -> b) -> b, String)]
class GRead t where
    greadsPrec :: Int -> GReadS t

greads :: GRead t => GReadS t
greads = greadsPrec (-1)

gread :: GRead t => String -> (forall a. t a -> b) -> b
gread s = hd [f | (f, "") <- greads s]
    where
        hd (x:_) = x
        hd _ = error "gread: No parse"