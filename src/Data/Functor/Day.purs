-- | The Day convolution of covariant functors.
-- |
-- | Based on <https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html>

module Data.Functor.Day
  ( Day
  , runDay
  , day
  , dap
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend, duplicate, extract)
import Control.Comonad.Trans (class ComonadTrans)
import Data.Exists (Exists, mkExists, runExists)

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

instance functorDay :: Functor (Day f g) where
  map f = runDay \get fx gy -> day (\x y -> f (get x y)) fx gy

instance extendDay :: (Extend f, Extend g) => Extend (Day f g) where
  extend f = map f <<< dup where
    dup = runDay \get fx gy -> day (day get) (duplicate fx) (duplicate gy)

instance comonadDay :: (Comonad f, Comonad g) => Comonad (Day f g) where
  extract = runDay \get fx gy -> get (extract fx) (extract gy)

instance comonadTrans :: Comonad f => ComonadTrans (Day f) where
  lower = runDay \get fx gy -> get (extract fx) <$> gy
