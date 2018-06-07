-- | The Day convolution of covariant functors.
-- |
-- | Based on <https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html>

module Data.Functor.Day
  ( Day
  , type (⊗)
  , runDay
  , day
  , dap
  , elimPair
  , pairDay
  , hoistDay1
  , hoistDay2
  , introDay1
  , introDay2
  , elimDay1
  , elimDay2
  , symmDay
  , assoclDay
  , assocrDay
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Trans.Class (class ComonadTrans)
import Control.Extend (class Extend, duplicate)
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Pairing (type (⋈))
import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..))

data Day1 f g a x y = Day1 (x -> y -> a) (f x) (g y)

data Day2 f g a x = Day2 (Exists (Day1 f g a x))

-- | Day convolution of two covariant functors
data Day f g a = Day (Exists (Day2 f g a))

infixl 6 type Day as ⊗

-- | Unpack a value of type `Day f g a`.
runDay :: forall f g a r. (forall x y. (x -> y -> a) -> f x -> g y -> r) -> Day f g a -> r
runDay f (Day e) = runExists (\(Day2 e1) -> runExists (\(Day1 get fx gy) -> f get fx gy) e1) e

-- | Construct a value of type `Day f g a`.
day :: forall f g a x y. (x -> y -> a) -> f x -> g y -> Day f g a
day get fx gy = Day (mkExists (Day2 (mkExists (Day1 get fx gy))))

-- | `f ⊗ f` whenever `f` is `Applicative`.
dap :: forall f. Applicative f => f ⊗ f ~> f
dap = runDay \get fx gy -> get <$> fx <*> gy

-- | Eliminate a `Day` convolution of two paired functors.
elimPair :: forall f g a. f ⋈ g -> Day f g a -> a
elimPair p = runDay p

-- | Pair two `Day` convolutions when their components pair.
pairDay :: forall f1 f2 g1 g2. f1 ⋈ f2 -> g1 ⋈ g2 -> f1 ⊗ g1 ⋈ f2 ⊗ g2
pairDay p1 p2 f day1 day2 =
  runDay (\g f1 g1 ->
    runDay (\h f2 g2 ->
      case p1 Tuple f1 f2, p2 Tuple g1 g2 of
        Tuple x1 x2, Tuple y1 y2 ->
          f (g x1 y1) (h x2 y2)) day2) day1

-- | Hoist a natural transformation over the left hand side of a 'Day' convolution.
hoistDay1 :: forall f g h. (f ~> g) -> f ⊗ h ~> g ⊗ h
hoistDay1 n = runDay \f x y -> day f (n x) y

-- | Hoist a natural transformation over the left hand side of a 'Day' convolution.
hoistDay2 :: forall f g h. (f ~> g) -> h ⊗ f ~> h ⊗ g
hoistDay2 n = runDay \f x y -> day f x (n y)

introDay1 :: forall f. f ~> Identity ⊗ f
introDay1 = day identity (Identity identity)

introDay2 :: forall f. f ~> f ⊗ Identity
introDay2 f = day (#) f (Identity identity)

elimDay1 :: forall f. Functor f => Identity ⊗ f ~> f
elimDay1 = runDay \f (Identity x) y -> f x <$> y

elimDay2 :: forall f. Functor f => f ⊗ Identity ~> f
elimDay2 = runDay \f x (Identity y) -> flip f y <$> x

symmDay :: forall f g. f ⊗ g ~> g ⊗ f
symmDay = runDay \f x y -> day (flip f) y x

assoclDay :: forall f g h. f ⊗ (g ⊗ h) ~> (f ⊗ g) ⊗ h
assoclDay = runDay \phi f x -> runDay (\psi g h -> day identity (day (\a b c -> phi a (psi b c)) f g) h) x

assocrDay :: forall f g h. (f ⊗ g) ⊗ h ~> f ⊗ (g ⊗ h)
assocrDay = runDay \phi x h -> runDay (\psi f g -> day (#) f (day (\a b c -> phi (psi c a) b) g h)) x

instance functorDay :: Functor (Day f g) where
  map f = runDay \get fx gy -> day (\x y -> f (get x y)) fx gy

instance applyDay :: (Apply f, Apply g) => Apply (Day f g) where
  apply f g =
    runDay (\get1 x1 y1 ->
      runDay (\get2 x2 y2 ->
        day (\(Tuple a1 a2) (Tuple b1 b2) -> (get1 a1 b1) (get2 a2 b2))
            (Tuple <$> x1 <*> x2)
            (Tuple <$> y1 <*> y2)) g) f

instance applicativeDay :: (Applicative f, Applicative g) => Applicative (Day f g) where
  pure a = day (\_ _ -> a) (pure unit) (pure unit)

instance extendDay :: (Extend f, Extend g) => Extend (Day f g) where
  extend f = map f <<< dup where
    dup = runDay \get fx gy -> day (day get) (duplicate fx) (duplicate gy)

instance comonadDay :: (Comonad f, Comonad g) => Comonad (Day f g) where
  extract = runDay \get fx gy -> get (extract fx) (extract gy)

instance comonadTrans :: Comonad f => ComonadTrans (Day f) where
  lower = runDay \get fx gy -> get (extract fx) <$> gy
