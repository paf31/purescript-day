module Data.Functor.Day.Profunctor
  ( class Profunctor
  , dimap
  , class Strong
  , first
  , second
  , NT(..)
  , runNT
  , Optic
  , Iso
  , Lens
  , lens
  , hoistOf
  ) where

import Prelude

import Data.Functor.Day (type (⊗), hoistDay1, hoistDay2, symmDay)
import Data.Functor.Day.Hom (Hom, evalHom)

-- | A profunctor in the category of functors and natural transformations.
class Profunctor p where
  dimap :: forall s t a b. (s ~> a) -> (b ~> t) -> p a b -> p s t

-- | A strong profunctor with respect to Day convolution.
class Profunctor p <= Strong p where
  first :: forall a b x. p a b -> p (a ⊗ x) (b ⊗ x)
  second :: forall a b x. p a b -> p (x ⊗ a) (x ⊗ b)

-- | Natural transformations form a `Strong` profunctor.
newtype NT f g = NT (f ~> g)

runNT :: forall f g. NT f g -> f ~> g
runNT (NT n) = n

instance profunctorNT :: Profunctor NT where
  dimap f g (NT n) = NT (f >>> n >>> g)

instance strongNT :: Strong NT where
  first (NT n) = NT (hoistDay1 n)
  second (NT n) = NT (hoistDay2 n)

-- | A general type for profunctor optics in the functor category.
type Optic p (s :: Type -> Type) (t :: Type -> Type) a b = p a b -> p s t

-- | An isomorphism of functors.
type Iso s t a b = forall p. Profunctor p => Optic p s t a b

-- | A lens with respect to Day convolution, which identifies
-- | one functor as a Day convolution of another with a third,
-- | unknown functor.
type Lens s t a b = forall p. Strong p => Optic p s t a b

-- | Create a profunctor lens from a getter-setter pair.
lens :: forall s t a b. Functor b => (s ~> a ⊗ Hom b t) -> Lens s t a b
lens f = first >>> dimap f (symmDay >>> evalHom)

-- | Hoist a natural transformation over the specified optic.
hoistOf :: forall s t a b. Optic NT s t a b -> (a ~> b) -> s ~> t
hoistOf o n = runNT (o (NT n))
