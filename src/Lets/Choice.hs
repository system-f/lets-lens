module Lets.Choice (
  Choice(..)
) where

import Lets.Data
import Lets.Profunctor

diswap ::
  Profunctor p =>
  p (Either a b) (Either c d)
  -> p (Either b a) (Either d c)
diswap =
  let swap = either Right Left 
  in dimap swap swap

-- | Map on left or right of @Either@. Only one of @left@ or @right@ needs to be
-- provided.
class Profunctor p => Choice p where
  left ::
    p a b
    -> p (Either a c) (Either b c)
  left =
    diswap . right

  right ::
    p a b
    -> p (Either c a) (Either c b)
  right =
    diswap . left

instance Choice (->) where
  left f =
    either (Left . f) Right
  right f =
    either Left (Right . f)

instance Choice Tagged where
  left (Tagged x) =
    Tagged (Left x)
  right (Tagged x) =
    Tagged (Right x)

