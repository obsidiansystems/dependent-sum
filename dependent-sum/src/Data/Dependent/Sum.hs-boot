{-# LANGUAGE ExistentialQuantification, TypeOperators #-}
module Data.Dependent.Sum where

data DSum tag f = forall a. !(tag a) :=> f a
infixr 1 :=>
