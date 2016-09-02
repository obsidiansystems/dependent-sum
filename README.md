dependent-sum [![Build Status](https://travis-ci.org/mokus0/dependent-sum.svg)](https://travis-ci.org/mokus0/dependent-sum)
==============

This library defines a dependent sum type:

    data DSum tag f = forall a. !(tag a) :=> f a

By analogy to the `key => value` construction for dictionary entries in many dynamic languages, we use `:=>` as the constructor for dependent sums.  The key is a tag that specifies the type of the value;  for example, think of a GADT such as:

    data Tag a where
       StringKey    :: Tag String
       IntKey       :: Tag Int

Then, we have the following valid expressions of type `DSum Tag []`:

    StringKey   => ["hello!"]
    IntKey      => [42]

And we can write functions that consume `DSum Tag` values by matching, such as:

    toString :: DSum Tag [] -> [String]
    toString (StringKey :=> strs) = strs
    toString (IntKey    :=> ints) = show ints

The `:=>` and `==>` operators have very low precedence and bind to the right, so if the `Tag` GADT is extended with an additional constructor `Rec :: Tag (DSum Tag Identity)`, then `Rec ==> AnInt ==> 3 + 4` is parsed as would be expected (`Rec ==> (AnInt ==> (3 + 4))`) and has type `DSum Identity Tag`.  Its precedence is just above that of `$`, so `foo bar $ AString ==> "eep"` is equivalent to `foo bar (AString ==> "eep")`.

In order to support basic type classes from the `Prelude` for `DSum`, there are also several type classes defined for "tag" types:

 - `GShow tag` means that `tag a` has (the equivalent of) a `Show` instance.
 - `ShowTag tag f` means that if `tag a` is inhabited (as witnessed by providing an instance), then `f a` has (the equivalent of) a `Show` instance.

There are similar classes for the `Prelude`'s `Eq`, `Ord` and `Read` classes.  Together, they provide the following instances for `DSum`:

    instance ShowTag tag f => Show (DSum tag f)
    instance ReadTag tag f => Read (DSum tag f)
    instance EqTag   tag f => Eq   (DSum tag f)
    instance OrdTag  tag f => Ord  (DSum tag f)

For example implementations of these classes, see the generated Haddock docs or the code in the `examples` directory.  There is a fair amount of boilerplate.  A few of the more common classes (`GEq`, `GCompare`, and `GShow`) can be automatically derived by Template Haskell code in the `dependent-sum-template` package.  It would be nice to implement more derivations (and it should be nearly as straightforward as deriving the Prelude classes they support), but I haven't done so yet.
