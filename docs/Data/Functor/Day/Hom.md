## Module Data.Functor.Day.Hom

The internal hom in the category of functors with Day
convolution as the monoidal tensor.

#### `Hom`

``` purescript
newtype Hom f g a
```

This is the internal hom in the category of functors with Day
convolution as the monoidal tensor.

##### Instances
``` purescript
(Comonad f) => MonadTrans (Hom f)
```

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

#### `introHom`

``` purescript
introHom :: forall f g h. ((Day f g) ~> h) -> f ~> g ~/> h
```

#### `elimHom`

``` purescript
elimHom :: forall f g h. Functor g => (f ~> g ~/> h) -> (Day f g) ~> h
```

#### `introHom'`

``` purescript
introHom' :: forall f g. Functor f => (f ~> g) -> Identity ~> f ~/> g
```

#### `elimHom'`

``` purescript
elimHom' :: forall f g. Functor f => (Identity ~> f ~/> g) -> f ~> g
```

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

#### `pairingHom`

``` purescript
pairingHom :: forall f g. f ⋈ g -> f ~> g ~/> Identity
```

`Hom` generalizes pairings which have been applied to their first argument.

#### `pairHom`

``` purescript
pairHom :: forall f. Functor f => f ⋈ (f ~/> Identity)
```

Every functor `f` pairs with `f ~/> Identity`.


