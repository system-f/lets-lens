module Lets.Profunctor (
  Profunctor(dimap)
, rmap
, lmap
) where

import Lets.Data

-- | A profunctor is a binary functor, with the first argument in contravariant
-- (negative) position and the second argument in covariant (positive) position.
class Profunctor p where
  dimap ::
    (b -> a)
    -> (c -> d)
    -> p a c
    -> p b d

instance Profunctor (->) where
  dimap f g = \h -> g . h . f

instance Profunctor Tagged where
  dimap _ g (Tagged x) =
    Tagged (g x)

rmap :: Profunctor p => (c -> d) -> p a c -> p a d
rmap = dimap id

lmap :: Profunctor p => (b -> a) -> p a c -> p b c
lmap = \x -> dimap x id

{-

class Bifunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

class Profunctor
  dimap :: (b -> a) -> (c -> d) -> f a c -> f b d

data Kleisli f a b = Kleisli (a -> f b)

instance Profunctor (->) where

(a -> b) -> (c -> d) -> (a -> c) -> b -> d

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

data Predicate a = Predicate (a -> Bool)

instance Contravariant Predicate where
  contramap f (Predicate g) =
    Predicate (g . f)

-- g :: a -> Bool
-- f :: b -> a
-- b :: b

mapPredicate :: (a -> b) -> Predicate a -> Predicate b
mapPredicate f (Predicate g) = Predicate (\b -> 
-- g :: a -> Bool
-- f :: a -> b
-- b :: b

-}
