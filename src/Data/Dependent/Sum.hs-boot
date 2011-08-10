{-# LANGUAGE ExistentialQuantification, TypeOperators #-}
module Data.Dependent.Sum where

data DSum tag = forall a. !(tag a) :=> a
infixr 1 :=>
