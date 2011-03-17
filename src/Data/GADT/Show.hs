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