-- | The Day convolution of covariant functors.
-- |
-- | Based on <https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html>

module Data.Functor.Day
  ( Day
  , runDay
  , day
  , dap
  , elimPair
  , pairDay
  , hoistDay1
  , hoistDay2
  , Hom
  , runHom
  , hom
  , curryHom
  , uncurryHom
  , composeHom
  , evalHom
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend, duplicate, extract)
import Control.Comonad.Trans (class ComonadTrans)
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Pairing (type (⋈))
import Data.Tuple (Tuple(..))

data Day1 f g a x y = Day1 (x -> y -> a) (f x) (g y)

data Day2 f g a x = Day2 (Exists (Day1 f g a x))

-- | Day convolution of two covariant functors
data Day f g a = Day (Exists (Day2 f g a))

-- | Unpack a value of type `Day f g a`.
runDay :: forall f g a r. (forall x y. (x -> y -> a) -> f x -> g y -> r) -> Day f g a -> r
runDay f (Day e) = runExists (\(Day2 e1) -> runExists (\(Day1 get fx gy) -> f get fx gy) e1) e

-- | Construct a value of type `Day f g a`.
day :: forall f g a x y. (x -> y -> a) -> f x -> g y -> Day f g a
day get fx gy = Day (mkExists (Day2 (mkExists (Day1 get fx gy))))

-- | Collapse a value of type `Day f f a` whenever `f` is `Applicative`.
dap :: forall f a. Applicative f => Day f f a -> f a
dap = runDay \get fx gy -> get <$> fx <*> gy

-- | Eliminate a `Day` convolution of two paired functors.
elimPair :: forall f g a. f ⋈ g -> Day f g a -> a
elimPair p = runDay p

-- | Pair two `Day` convolutions when their components pair.
pairDay :: forall f1 f2 g1 g2. f1 ⋈ f2 -> g1 ⋈ g2 -> Day f1 g1 ⋈ Day f2 g2
pairDay p1 p2 f day1 day2 =
  runDay (\g f1 g1 ->
    runDay (\h f2 g2 ->
      case p1 Tuple f1 f2, p2 Tuple g1 g2 of
        Tuple x1 x2, Tuple y1 y2 ->
          f (g x1 y1) (h x2 y2)) day2) day1

-- | Hoist a natural transformation over the left hand side of a 'Day' convolution.
hoistDay1 :: forall f g h. (f ~> g) -> Day f h ~> Day g h
hoistDay1 n = runDay \f x y -> day f (n x) y

-- | Hoist a natural transformation over the left hand side of a 'Day' convolution.
hoistDay2 :: forall f g h. (f ~> g) -> Day h f ~> Day h g
hoistDay2 n = runDay \f x y -> day f x (n y)

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

-- | This is the internal hom in the category of functors with Day
-- | convolution as the monoidal tensor.
newtype Hom f g a = Hom (forall r. f (a -> r) -> g r)

infixr 8 type Hom as ~/>

hom :: forall f g a. (forall r. f (a -> r) -> g r) -> Hom f g a
hom = Hom

runHom :: forall f g a r. Hom f g a -> f (a -> r) -> g r
runHom (Hom f) = f

instance functorHom :: Functor f => Functor (f ~/> g) where
  map f d = Hom \fa -> runHom d (map (_ <<< f) fa)

-- | The curry function for the internal hom object `Hom`
curryHom :: forall f g h. Day f g ~/> h ~> f ~/> g ~/> h
curryHom (Hom d) = Hom \f -> Hom \g -> d (day (>>>) f g)

-- | The uncurry function for the internal hom object `Hom`
uncurryHom :: forall f g h. (Functor f, Functor g) => f ~/> g ~/> h ~> Day f g ~/> h
uncurryHom d = Hom (runDay \f x y -> runHom (runHom d (map (\p q a -> f p a q) x)) (map (#) y))

-- | The composition map for the internal hom object `Hom`
composeHom :: forall f g h. Functor f => Day (g ~/> h) (f ~/> g) ~> f ~/> h
composeHom = runDay \f gh fg -> Hom \fa -> runHom gh (runHom fg (map (\g y x -> g (f x y)) fa))

-- | The evaluation map for the internal hom object `Hom`
evalHom :: forall f g. Functor f => Day (f ~/> g) f ~> g
evalHom = runDay \f d y -> runHom d (map (flip f) y)
