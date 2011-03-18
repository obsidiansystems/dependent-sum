{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Data.GADT.Show where

-- |'Show'-like class for 1-type-parameter GADTs
class GShow t where
    gshowsPrec :: Int -> t a -> ShowS


gshows :: GShow t => t a -> ShowS
gshows = gshowsPrec (-1)

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
        hd _ = error "gread: no parse"