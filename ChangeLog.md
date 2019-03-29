# Revision history for dependent-sum

## 0.5.1.0

* Add `mkSome` and `mapSome` to `Data.Some`.
* Add `GEq`, `GCompare`, `GShow,` and `GRead` instances for `Sum` and `Product`
  (Except `GRead (Product a b)`)

## 0.5.0.0

* Make `Some` a `newtype` with associated pattern synonyms using `unsafeCoerce`
  to avoid the GADT performance overhead. This shouldn't affect users.
* Deprecate the constructor name `This` in favor of `Some`.
* Drop support for GHC older than 8.0.
