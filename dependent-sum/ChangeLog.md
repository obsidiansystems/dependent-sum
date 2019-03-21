# Revision history for dependent-sum

## 0.5 - 2019-03-21

* Use constraints-extras ArgDict/Has' to define the instances of Eq, Ord, Read and Show for DSum. This obviates the need for the EqTag, OrdTag, ReadTag and ShowTag classes.
* Add instances of GEq and GCompare for functor Sum and Product.
* Add mapSome :: (forall a. f a -> g a) -> Some f -> Some g
* Remove support for GHC 7.x
* The git repositories for dependent-sum and dependent-sum-template are now the same, though the Haskell packages remain separate.