-- | The internal hom in the category of functors with Day
-- | convolution as the monoidal tensor.

module Data.Functor.Day.Hom
  ( Hom
  , type (⊸)
  , runHom
  , hom
  , curryHom
  , uncurryHom
  , composeHom
  , evalHom
  , introHom
  , introHom'
  , elimHom
  , elimHom'
  , pairingHom
  , pairHom
  ) where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, (=>>))
import Control.Monad.Trans.Class (class MonadTrans)
import Data.Functor.Day (type (⊗), runDay, day)
import Data.Functor.Pairing (type (⋈))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

-- | This is the internal hom in the category of functors with Day
-- | convolution as the monoidal tensor.
newtype Hom f g a = Hom (forall r. f (a -> r) -> g r)

infixr 5 type Hom as ⊸

hom :: forall f g a. (forall r. f (a -> r) -> g r) -> Hom f g a
hom = Hom

runHom :: forall f g a r. Hom f g a -> f (a -> r) -> g r
runHom (Hom f) = f

-- | The curry function for the internal hom object `Hom`
curryHom :: forall f g h. (f ⊗ g ⊸ h) ~> f ⊸ g ⊸ h
curryHom (Hom d) = Hom \f -> Hom \g -> d (day (>>>) f g)

-- | The uncurry function for the internal hom object `Hom`
uncurryHom :: forall f g h. Functor f => Functor g => (f ⊸ g ⊸ h) ~> f ⊗ g ⊸ h
uncurryHom d = Hom (runDay \f x y -> runHom (runHom d (map (\p q a -> f p a q) x)) (map (#) y))

introHom :: forall f g h. (f ⊗ g ~> h) -> f ~> g ⊸ h
introHom n f = Hom \g -> n (day (#) f g)

elimHom :: forall f g h. Functor g => (f ~> g ⊸ h) -> f ⊗ g ~> h
elimHom n = runDay \f x y -> runHom (n x) (map (flip f) y)

introHom' :: forall f g. Functor f => (f ~> g) -> Identity ~> f ⊸ g
introHom' n = introHom (runDay \f (Identity a) x -> n (map (f a) x))

elimHom' :: forall f g. Functor f => (Identity ~> f ⊸ g) -> f ~> g
elimHom' n fa = elimHom n (day (const identity) (Identity unit) fa)

-- | The composition map for the internal hom object `Hom`
composeHom :: forall f g h. Functor f => (g ⊸ h) ⊗ (f ⊸ g) ~> f ⊸ h
composeHom = runDay \f gh fg -> Hom \fa -> runHom gh (runHom fg (map (\g y x -> g (f x y)) fa))

-- | The evaluation map for the internal hom object `Hom`
evalHom :: forall f g. Functor f => (f ⊸ g) ⊗ f ~> g
evalHom = runDay \f d y -> runHom d (map (flip f) y)

-- | `Hom` generalizes pairings which have been applied to their first argument.
pairingHom :: forall f g. f ⋈ g -> f ~> g ⊸ Identity
pairingHom p = introHom (runDay \f x y -> Identity (p f x y))

-- | Every functor `f` pairs with `f ⊸ Identity`.
pairHom :: forall f. Functor f => f ⋈ (f ⊸ Identity)
pairHom f fa h = unwrap (runHom h (map f fa))

instance functorHom :: Functor f => Functor (Hom f g) where
  map f d = Hom \fa -> runHom d (map (_ <<< f) fa)

instance applyHom :: (Extend f, Bind g) => Apply (Hom f g) where
  apply (Hom f) (Hom a) = Hom \w -> join (f (w =>> \wf g -> a (map (_ <<< g) wf)))

instance applicativeHom :: (Comonad f, Monad g) => Applicative (Hom f g) where
  pure a = Hom \w -> pure (extract w a)

instance bindHom :: (Comonad f, Monad g) => Bind (Hom f g) where
  bind h f = Hom \w -> join (runHom h (w =>> \fk a -> runHom (f a) fk))

instance monadHom :: (Comonad f, Monad g) => Monad (Hom f g)

instance monadTrans :: Comonad f => MonadTrans (Hom f) where
  lift ma = Hom \f -> map (extract f) ma
