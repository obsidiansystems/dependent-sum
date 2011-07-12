This library defines a dependent sum type:

    data DSum tag = forall a. !(tag a) :=> a

By analogy to the `key => value` construction for dictionary entries in many dynamic languages, we use `key :=> value` as the constructor for dependent sums.  The key is a tag that specifies the type of the value;  for example, think of a GADT such as:

    data Tag a where
       AString :: Tag String
       AnInt   :: Tag Int

Then, we have the following valid expressions of type `DSum Tag`:

    AString :=> "hello!"
    AnInt   :=> 42

And we can write functions that consume `DSum Tag` values by matching, such as:

    toString :: DSum Tag -> String
    toString (AString :=> str) = str
    toString (AnInt   :=> int) = show int

The `:=>` operator has very low precedence and binds to the right, so if the `Tag` GADT is extended with an additional constructor `Rec :: Tag (DSum Tag)`, then `Rec :=> AnInt :=> 3 + 4` is parsed as would be expected (`Rec :=> (AnInt :=> (3 + 4))`) and has type `DSum Tag`. Its precedence is just above that of `$`, so `foo bar $ AString :=> "eep"` is equivalent to `foo bar (AString :=> "eep")`.

In order to support basic type classes from the `Prelude` for `DSum`, there are also several type classes defined for "tag" types:

 - `GShow tag` means that `tag a` has (the equivalent of) a `Show` instance.
 - `ShowTag tag` means that if `tag a` is inhabited (as witnessed by providing an instance), then `a` has (the equivalent of) a `Show` instance.

There are similar classes for the `Prelude`'s `Eq`, `Ord` and `Read` classes.  Together, they provide the following instances for `DSum`:

    instance ShowTag tag => Show (DSum tag)
    instance ReadTag tag => Read (DSum tag)
    instance EqTag   tag => Eq   (DSum tag)
    instance OrdTag  tag => Ord  (DSum tag)

For example implementations of these classes, see the generated Haddock docs or the code in the `examples` directory.  There is a fair amount of boilerplate.  It would be nice to implement some Template Haskell code to derive these classes (it would be just as straightforward as deriving the Prelude classes they support), but I haven't done so yet.
