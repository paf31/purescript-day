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

#### `runDay`

``` purescript
runDay :: forall f g a r. (forall y. forall x. (x -> y -> a) -> f x -> g y -> r) -> Day f g a -> r
```

Unpack a value of type `Day f g a`.

#### `day`

``` purescript
day :: forall f g a x y. (x -> y -> a) -> f x -> g y -> Day f g a
```

Construct a value of type `Day f g a`.

#### `dap`

``` purescript
dap :: forall f a. Applicative f => Day f f a -> f a
```

Collapse a value of type `Day f f a` whenever `f` is `Applicative`.

#### `elimPair`

``` purescript
elimPair :: forall f g a. f ⋈ g -> Day f g a -> a
```

Eliminate a `Day` convolution of two paired functors.

#### `pairDay`

``` purescript
pairDay :: forall f1 f2 g1 g2. f1 ⋈ f2 -> g1 ⋈ g2 -> (Day f1 g1) ⋈ (Day f2 g2)
```

Pair two `Day` convolutions when their components pair.

#### `hoistDay1`

``` purescript
hoistDay1 :: forall f g h. (f ~> g) -> (Day f h) ~> (Day g h)
```

Hoist a natural transformation over the left hand side of a 'Day' convolution.

#### `hoistDay2`

``` purescript
hoistDay2 :: forall f g h. (f ~> g) -> (Day h f) ~> (Day h g)
```

Hoist a natural transformation over the left hand side of a 'Day' convolution.

#### `Hom`

``` purescript
newtype Hom f g a
```

This is the internal hom in the category of functors with Day
convolution as the monoidal tensor.

#### `hom`

``` purescript
hom :: forall f g a. (forall r. f (a -> r) -> g r) -> Hom f g a
```

#### `runHom`

``` purescript
runHom :: forall f g a r. Hom f g a -> f (a -> r) -> g r
```

#### `curryHom`

``` purescript
curryHom :: forall f g h. (Day f g) ~/> h ~> f ~/> g ~/> h
```

The curry function for the internal hom object `Hom`

#### `uncurryHom`

``` purescript
uncurryHom :: forall f g h. (Functor f, Functor g) => f ~/> g ~/> h ~> (Day f g) ~/> h
```

The uncurry function for the internal hom object `Hom`

#### `composeHom`

``` purescript
composeHom :: forall f g h. Functor f => (Day (g ~/> h) (f ~/> g)) ~> f ~/> h
```

The composition map for the internal hom object `Hom`

#### `evalHom`

``` purescript
evalHom :: forall f g. Functor f => (Day (f ~/> g) f) ~> g
```

The evaluation map for the internal hom object `Hom`


