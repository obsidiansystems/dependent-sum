{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PolyKinds #-}
#endif
module Data.GADT.Show where

-- |'Show'-like class for 1-type-parameter GADTs.  @GShow t => ...@ is equivalent to something
-- like @(forall a. Show (t a)) => ...@.  The easiest way to create instances would probably be
-- to write (or derive) an @instance Show (T a)@, and then simply say:
--
-- > instance GShow t where gshowsPrec = showsPrec
class GShow t where
    gshowsPrec :: Int -> t a -> ShowS


gshows :: GShow t => t a -> ShowS
gshows = gshowsPrec (-1)

gshow :: (GShow t) => t a -> String
gshow x = gshows x ""

data GReadResult t where
  GReadResult :: {getGReadResult :: t a} -> GReadResult t

type GReadS t = ReadS (GReadResult t)

-- |'Read'-like class for 1-type-parameter GADTs.  Unlike 'GShow', this one cannot be 
-- mechanically derived from a 'Read' instance because 'greadsPrec' must choose the phantom
-- type based on the 'String' being parsed.
class GRead t where
    greadsPrec :: Int -> GReadS t

greads :: GRead t => GReadS t
greads = greadsPrec (-1)

gread :: GRead t => String -> (forall a. t a -> b) -> b
gread s g = case hd [f | (f, "") <- greads s] of
              GReadResult res -> g res
    where
        hd (x:_) = x
        hd _ = error "gread: no parse"
