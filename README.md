# th-to-exp [![Build Status](https://travis-ci.org/lexi-lambda/th-to-exp.svg?branch=master)](https://travis-ci.org/lexi-lambda/th-to-exp)

`th-to-exp` is a package that provides a way to persist data from compile-time to runtime by producing Template Haskell expressions that evaluate to particular values. For example, if you have a value `Just 1`, then `toExp (Just 1)` will produce the *expression* `[e| Just 1 |]`, which can be used in a splice. For a more direct example, here’s what that looks like without the quasiquote notation:

```haskell
> toExp (Just 1)
AppE (ConE GHC.Base.Just) (LitE (IntegerL 1))
```

This is done by using a typeclass, `ToExp`, that can be automatically derived for types that have a `Generic` instance.
