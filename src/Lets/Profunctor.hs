module Lets.Profunctor (
  Profunctor(dimap)
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
