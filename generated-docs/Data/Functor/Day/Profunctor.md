## Module Data.Functor.Day.Profunctor

#### `Profunctor`

``` purescript
class Profunctor p  where
  dimap :: forall s t a b. (s ~> a) -> (b ~> t) -> p a b -> p s t
```

A profunctor in the category of functors and natural transformations.

##### Instances
``` purescript
Profunctor NT
```

#### `Strong`

``` purescript
class (Profunctor p) <= Strong p  where
  first :: forall a b x. p a b -> p (a ⊗ x) (b ⊗ x)
  second :: forall a b x. p a b -> p (x ⊗ a) (x ⊗ b)
```

A strong profunctor with respect to Day convolution.

##### Instances
``` purescript
Strong NT
```

#### `NT`

``` purescript
newtype NT f g
  = NT (f ~> g)
```

Natural transformations form a `Strong` profunctor.

##### Instances
``` purescript
Profunctor NT
Strong NT
```

#### `runNT`

``` purescript
runNT :: forall f g. NT f g -> f ~> g
```

#### `Optic`

``` purescript
type Optic p (s :: Type -> Type) (t :: Type -> Type) a b = p a b -> p s t
```

A general type for profunctor optics in the functor category.

#### `Iso`

``` purescript
type Iso s t a b = forall p. Profunctor p => Optic p s t a b
```

An isomorphism of functors.

#### `Lens`

``` purescript
type Lens s t a b = forall p. Strong p => Optic p s t a b
```

A lens with respect to Day convolution, which identifies
one functor as a Day convolution of another with a third,
unknown functor.

#### `lens`

``` purescript
lens :: forall s t a b. Functor b => (s ~> a ⊗ (Hom b t)) -> Lens s t a b
```

Create a profunctor lens from a getter-setter pair.

#### `hoistOf`

``` purescript
hoistOf :: forall s t a b. Optic NT s t a b -> (a ~> b) -> s ~> t
```

Hoist a natural transformation over the specified optic.


