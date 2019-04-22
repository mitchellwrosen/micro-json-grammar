#### Boolean

```haskell
>>> forwards boolean (≪true≫, ())
Just (True, ())

>>> backwards boolean (True, ())
Just (≪true≫, ())

>>> forwards true (≪true≫, ())
Just ()

>>> backwards true ()
Just (≪true≫, ())
```

#### Integral

```haskell
>>> forwards integral (≪1≫, ())
Just (1, ())

>>> backwards integral (1, ())
Just (≪1.0≫, ())
```

#### Floating

```haskell
>>> forwards floating (≪1.5≫, ())
Just (1.5, ())

>>> backwards floating (1.5, ())
Just (≪1.5≫, ())
```

#### String

```haskell
>>> forwards string (≪"foo"≫, ())
Just ("foo", ())

>>> backwards string ("foo", ())
Just (≪"foo"≫, ())

>>> forwards (symbol "foo") (≪"foo"≫, ())
Just ()

>>> backwards (symbol "foo") ()
Just (≪"foo"≫, ())
```

#### Object

```haskell
>>> forwards (
      lenientObject (
        key "foo" boolean >>>
        key "bar" string
      )
    )
    (≪{"foo": true, "bar": "baz"}≫, ())
Just ("baz", (True, ()))

>>> backwards (
      lenientObject (
        key "foo" boolean >>>
        key "bar" string
      )
    )
    ("baz", (True, ()))
Just (≪{"foo": true, "bar": "baz"}≫, ())
```

#### Array

```haskell
>>> forwards (array boolean) (≪[true, false]≫, ())
Just ([True, False], ())

>>> backwards (array boolean) (Vector.fromList [True, False], ())
Just (≪[true, false]≫, ())
```

#### Tuple

```haskell
>>> forwards (
      tuple (
        element (symbol "foo") >>>
        element boolean
      )
    )
    (≪["foo", true]≫, ())
Just (True, ())

>>> backwards (
      tuple (
        element (symbol "foo") >>>
        element boolean
      )
    )
    (True, ())
Just (≪["foo", true]≫, ())
```
