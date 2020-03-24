dependent-sum [![Build Status](https://travis-ci.org/obsidiansystems/dependent-sum.svg)](https://travis-ci.org/obsidiansystems/dependent-sum)
==============

This library defines a dependent sum type:

    data DSum tag f = forall a. !(tag a) :=> f a

By analogy to the `key => value` construction for dictionary entries in many dynamic languages, we use `:=>` as the constructor for dependent sums.  The key is a tag that specifies the type of the value;  for example, think of a GADT such as:

    data Tag a where
       StringKey    :: Tag String
       IntKey       :: Tag Int

Then, we have the following valid expressions of type `DSum Tag []`:

    StringKey   :=> ["hello!"]
    IntKey      :=> [42]

And we can write functions that consume `DSum Tag` values by matching, such as:

    toString :: DSum Tag [] -> [String]
    toString (StringKey :=> strs) = strs
    toString (IntKey    :=> ints) = show ints

The `:=>` and `==>` operators have very low precedence and bind to the right, so if the `Tag` GADT is extended with an additional constructor `Rec :: Tag (DSum Tag Identity)`, then `Rec ==> AnInt ==> 3 + 4` is parsed as would be expected (`Rec ==> (AnInt ==> (3 + 4))`) and has type `DSum Identity Tag`.  Its precedence is just above that of `$`, so `foo bar $ AString ==> "eep"` is equivalent to `foo bar (AString ==> "eep")`.

In order to support basic type classes from the `Prelude` for `DSum`, there are also several type classes defined for "tag" types:

 - `GEq tag` is similar to an `Eq` instance for `tag a` except that with `geq`, values of types `tag a` and `tag b` may be compared, and in the case of equality, evidence that the types `a` and `b` are equal is provided.
 - `GCompare tag` is similar to the above for `Ord`, and provides `gcompare`, giving a `GOrdering` that gives similar evidence of type equality when values match.
 - `GShow tag` means that `tag a` has (the equivalent of) a `Show` instance.
 - `GRead tag` means that `tag a` has (the equivalent of) a `Read` instance.

In order to be able to compare values of type `DSum tag f` for equality, in addition to having a `GEq tag` instance, we need to know that, given a value `t :: tag a`, we may obtain an instance `Eq (f a)`, which is expressed by the use of the `Has' Eq tag f` constraint from the constraints-extras package, so we have the following instances:

    (GEq tag, Has' Eq tag f) => Eq (DSum tag f)
    (GCompare tag, Has' Eq tag f, Has' Ord tag f) => Ord (DSum tag f)
    (GShow tag, Has' Show tag f) => Show (DSum tag f)
    (GRead tag, Has' Read tag f) => Read (DSum tag f)

In order to satisfy the `Has'` constraints, you'll want to use `deriveArgDict` from constraints-extras, or less-commonly, write your own instance of the ArgDict class by hand, in addition to making sure that it's actually the case that for every value of your tag type, there will be a corresponding instance of Eq/Ord/Read/Show as appropriate.

For example implementations of these classes, see the generated Haddock docs or the code in the `examples` directory.  There is a fair amount of boilerplate.  A few of the more common classes (`GEq`, `GCompare`, and `GShow`) can be automatically derived by Template Haskell code in the `dependent-sum-template` package.
