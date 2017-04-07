## Module Data.Functor.Day

The Day convolution of covariant functors.

Based on <https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html>

#### `Day`

``` purescript
data Day f g a
```

Day convolution of two covariant functors

##### Instances
``` purescript
Functor (Day f g)
(Apply f, Apply g) => Apply (Day f g)
(Applicative f, Applicative g) => Applicative (Day f g)
(Extend f, Extend g) => Extend (Day f g)
(Comonad f, Comonad g) => Comonad (Day f g)
(Comonad f) => ComonadTrans (Day f)
```

#### `type (⊗)`

``` purescript
infixl 6 type Day as ype (⊗
```

#### `runDay`

``` purescript
runDay :: forall f g a r. (forall x y. (x -> y -> a) -> f x -> g y -> r) -> Day f g a -> r
```

Unpack a value of type `Day f g a`.

#### `day`

``` purescript
day :: forall f g a x y. (x -> y -> a) -> f x -> g y -> Day f g a
```

Construct a value of type `Day f g a`.

#### `dap`

``` purescript
dap :: forall f. Applicative f => f ⊗ f ~> f
```

`f ⊗ f` whenever `f` is `Applicative`.

#### `elimPair`

``` purescript
elimPair :: forall f g a. f ⋈ g -> Day f g a -> a
```

Eliminate a `Day` convolution of two paired functors.

#### `pairDay`

``` purescript
pairDay :: forall f1 f2 g1 g2. f1 ⋈ f2 -> g1 ⋈ g2 -> f1 ⊗ g1 ⋈ f2 ⊗ g2
```

Pair two `Day` convolutions when their components pair.

#### `hoistDay1`

``` purescript
hoistDay1 :: forall f g h. (f ~> g) -> f ⊗ h ~> g ⊗ h
```

Hoist a natural transformation over the left hand side of a 'Day' convolution.

#### `hoistDay2`

``` purescript
hoistDay2 :: forall f g h. (f ~> g) -> h ⊗ f ~> h ⊗ g
```

Hoist a natural transformation over the left hand side of a 'Day' convolution.


