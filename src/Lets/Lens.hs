{-# LANGUAGE RankNTypes #-}

module Lets.Lens (
  Persan(..), persanStrings, freddy, Json(..), _JsonObject, _JsonArray,
  fmapT
, over
, fmapTAgain
, Set
, sets
, mapped
, set
, foldMapT
, foldMapOf
, foldMapTAgain
, Fold
, folds
, folded
, Get
, get
, Traversal
, both
, traverseLeft
, traverseRight
, Traversal'
, Lens
, Prism
, _Left
, _Right
, prism
, _Just
, _Nothing
, setP
, getP
, Prism'
, modify
, (%~)
, (.~)
, fmodify
, (|=)
, fstL
, sndL
, mapL
, setL
, compose
, (|.)
, identity
, product
, (***)
, choice
, (|||)
, Lens'
, cityL
, stateL
, countryL
, streetL
, suburbL
, localityL
, ageL
, nameL
, addressL
, intAndIntL
, intAndL
, getSuburb
, setStreet
, getAgeAndCountry
, setCityAndLocality
, getSuburbOrCity
, setStreetOrState
, modifyCityUppercase
, modifyIntAndLengthEven
, traverseLocality
, intOrIntP
, intOrP
, intOrLengthEven
) where

import Control.Applicative(Applicative((<*>), pure), liftA2)
import Data.Char(toUpper)
import Data.Foldable(Foldable(foldMap))
import Data.Functor((<$>))
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Monoid(Monoid)
import qualified Data.Set as Set(Set, insert, delete, member)
import Data.Traversable(Traversable(traverse))
import Lets.Data(AlongsideLeft(AlongsideLeft, getAlongsideLeft), AlongsideRight(AlongsideRight, getAlongsideRight), Identity(Identity, getIdentity), Const(Const, getConst), Tagged(Tagged, getTagged), IntOr(IntOrIs, IntOrIsNot), IntAnd(IntAnd), Person(Person), Locality(Locality), Address(Address), bool)
import Lets.Choice(Choice(left, right))
import Lets.Profunctor(Profunctor(dimap))
import Prelude hiding (product)

-- $setup
-- >>> import qualified Data.Map as Map(fromList)
-- >>> import qualified Data.Set as Set(fromList)
-- >>> import Data.Char(ord)
-- >>> import Lets.Data

-- Let's remind ourselves of Traversable, noting Foldable and Functor.
--
-- class (Foldable t, Functor t) => Traversable t where
--   traverse ::
--     Applicative f => 
--     (a -> f b)
--     -> t a
--     -> f (t b)

-- | Observe that @fmap@ can be recovered from @traverse@ using @Identity@.
--
-- /Reminder:/ fmap :: Functor t => (a -> b) -> t a -> t b
fmapT ::
  Traversable t =>
  (a -> b)
  -> t a
  -> t b
fmapT =
  \k -> getIdentity . traverse (Identity . k)

-- | Let's refactor out the call to @traverse@ as an argument to @fmapT@.
over :: 
  ((a -> Identity b) -> s -> Identity t)
  -> (a -> b)
  -> s
  -> t
over =
  \t k -> getIdentity . t (Identity . k)

-- | Here is @fmapT@ again, passing @traverse@ to @over@.
fmapTAgain ::
  Traversable t =>
  (a -> b)
  -> t a
  -> t b
fmapTAgain = over traverse
 -- error "todo: fmapTAgain"

-- | Let's create a type-alias for this type of function.
type Set s t a b =
  (a -> Identity b)
  -> s
  -> Identity t

-- | Let's write an inverse to @over@ that does the @Identity@ wrapping &
-- unwrapping.
sets ::
  ((a -> b) -> s -> t)
  -> (a -> Identity b)
  -> s
  -> Identity t  
sets t k = Identity . t (getIdentity . k)
-- over t k = getIdentity . t (Identity . k)

-- over (sets f) = f
-- sets (over f) = f

mapped ::
  Functor f =>
  Set (f a) (f b) a b
--(a -> Identity b) -> f a -> Identity (f b)
mapped =
  sets fmap

-- (a -> b) -> f a -> f b

{-
type Set s t a b =
  (a -> Identity b)
  -> s
  -> Identity t
-}
set ::
  -- (Set s t a b)
  ((a -> Identity b) -> s -> Identity t)
  -> s
  -> b
  -> t
set q s b =
  over q (const b) s

----

-- | Observe that @foldMap@ can be recovered from @traverse@ using @Const@.
--
-- /Reminder:/ foldMap :: (Foldable t, Monoid b) => (a -> b) -> t a -> b
foldMapT ::
  (Traversable t, Monoid m) =>
  (a -> m)
  -> t a
  -> m
foldMapT =
  \k -> getConst . traverse (Const . k)
-- traverse :: (a -> f b) -> t a -> f (t b)
-- traverse :: (a -> Identity b) -> t a -> Identity (t b)
-- traverse :: (a -> Const m b) -> t a -> Const m (t b)


-- 

--  foldMapDefault f = getConst . traverse (Const . f)

-- newtype Const m a = Const { getConst :: m }
-- instance Monoid m => Applicative (Const m) where
--   pure _ = Const mempty
--   Const m <*> Const n = Const (mappend m n)
 
-- | Let's refactor out the call to @traverse@ as an argument to @foldMapT@.
foldMapOf ::
  ((a -> Const r b) -> s -> Const r t)
  -> (a -> r)
  -> s
  -> r
foldMapOf =
  \t k -> getConst . t (Const . k)

-- | Here is @foldMapT@ again, passing @traverse@ to @foldMapOf@.
foldMapTAgain ::
  (Traversable t, Monoid b) =>
  (a -> b)
  -> t a
  -> b
foldMapTAgain =
  foldMapOf traverse

-- | Let's create a type-alias for this type of function.
type Fold s t a b =
  forall r.
  Monoid r =>
  (a -> Const r b)
  -> s
  -> Const r t

-- | Let's write an inverse to @foldMapOf@ that does the @Const@ wrapping &
-- unwrapping.
folds ::
  ((a -> b) -> s -> t)
  -> (a -> Const b a)
  -> s
  -> Const t s
folds t k = Const . t (getConst . k)

folded ::
  Foldable f =>
  Fold (f a) (f a) a a
folded =
  folds foldMap

----

-- | @Get@ is like @Fold@, but without the @Monoid@ constraint.
type Get r s a =
  (a -> Const r a)
  -> s
  -> Const r s

get :: -- aka view
  ((a -> Const a a) -> s -> Const a s)
  -> s
  -> a
get =
  \f s -> getConst (f Const s)

----

-- get :: Lens s t a b -> s -> a
-- set :: Lens s t a b -> s -> b -> t


-- get :: Lens' s a -> (s -> a)
-- set :: Lens' s a -> (s -> a -> s)
{-
fmap id = id
fmap f . fmap g = fmap (f . g)

over l id = id
over l f . over l g = over l (f . g)


set l s (get l s) = s
get l (set l s a) = a
set l (set l s a) b = set l s b
-}

-- foo { bar = bar foo + 1 }
-- foo { bar = bar foo { baz = .. 

-- bar.baz +~ 1




-- | Let's generalise @Identity@ and @Const r@ to any @Applicative@ instance.
type Traversal s t a b =
  forall f.
  Applicative f =>
  (a -> f b)
  -> s
  -> f t

-- | Traverse both sides of a pair.
both ::
  Applicative f => (a -> f b) -> (a, a) -> f (b, b)
  -- Traversal (a, a) (b, b) a b
both f (a1, a2) =
  liftA2' (,) (f a1) (f a2)
--(,) <$> f a1 <*> f a2

-- liftA2' (,) :: f b -> f b -> f (b, b)

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = 
  f <$> fa <*> fb

-- liftA2' (,) :: f a -> f b -> f (a, b)
-- liftA2' (,) :: f b -> f b -> f (b, b)
-- f :: a -> b -> c
-- fa :: f a
-- f <$> fa :: f (b -> c)

-- fb :: f b
-- (<*>) :: forall x y. f (x -> y) -> f x -> f y
-- f <$> fa <*> fb :: f c
-- ?  :: f c


-- | Traverse the left side of @Either@.
traverseLeft ::
  Applicative f =>
  (a -> f b)
  -> Either a x
  -> f (Either b x)
--  Traversal (Either a x) (Either b x) a b
{-
traverseLeft k (Left a) = Left <$> k a
traverseLeft _ (Right x) = pure (Right x)
-}
-- traverseLeft f = fmap swapE . traverse f . swapE

traverseLeft = swapped.traverse

swapped :: Functor f => (Either a b -> f (Either c d)) -> Either b a -> f (Either d c)
swapped f = fmap swapE . f . swapE

swapE :: Either a b -> Either b a
swapE (Left a) = Right a
swapE (Right b) = Left b

-- | Traverse the right side of @Either@.
traverseRight ::
  Applicative f =>
  (a -> f b)
  -> Either x a
  -> f (Either x b)
  -- Traversal (Either x a) (Either x b) a b
traverseRight _ (Left x) = pure (Left x)
traverseRight k (Right a) = Right <$> k a

type Traversal' s a =
  Traversal s s a a

data Persan =
  Persan {
    _firstName :: String -- firstName
  , _middleName :: String -- middle name
  , _surname :: String -- surname
  , _age :: Int -- age
  } deriving (Eq, Show)

makeClassy ''Persan

class HasPersan t where
  persan :: Lens' t Persan
  firstName :: Lens' t String
  middleName :: Lens' t String
  surname :: Lens' t String
  age :: Lens' t Int

instance HasPersan Persan where
  persan = id

data ImportantPersan = ImportantPersan
  { _importantPersanTitle :: String
  , _importantPersanPersan :: Persan
  }

makeClassy ''ImportantPersan
instance HasPersan ImportantPersan where
  persan = importantPersanPersan

freddy :: Persan
freddy = Persan "fred" "fredly" "fredness" 123

persanStrings :: Traversal' Persan String
-- Applicative f => (String -> f String) -> Persan -> f Persan
persanStrings k (Persan f m s a) =
  (\f' m' s' -> Persan f' m' s' a) <$> k f <*> k m <*> k s

----

-- | @Const r@ is @Applicative@, if @Monoid r@, however, without the @Monoid@
-- constraint (as in @Get@), the only shared abstraction between @Identity@ and
-- @Const r@ is @Functor@.
--
-- Consequently, we arrive at our lens derivation:
type Lens s t a b =
  forall f.
  Functor f =>
  (a -> f b)
  -> s
  -> f t

----

-- | A prism is a less specific type of traversal.
type Prism s t a b =
  forall p f.
  (Choice p, Applicative f) =>
  p a (f b)
  -> p s (f t)

_Left ::
  Prism (Either a x) (Either b x) a b
_Left =
  error "todo: _Left"

_Right ::
  Prism (Either x a) (Either x b) a b 
_Right =
  error "todo: _Right"

prism ::
  (b -> t)
  -> (s -> Either t a)
  -> Prism s t a b
prism to fr =
  dimap fr (either pure (fmap to)) . right

_Just ::
  Prism (Maybe a) (Maybe b) a b
_Just =
  error "todo: _Just"

_Nothing ::
  Prism (Maybe a) (Maybe a) () ()
_Nothing =
  error "todo: _Nothing"


data Json =
  JsonArray [Json]
  | JsonObject (Map String Json)
  | JsonString String
  | JsonNumber Double
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

makePrisms ''Json
{-
_JsonObject :: Prism' Json (Map String Json)
_JsonObject =
  prism
    JsonObject
    (\j -> case j of
             JsonObject m -> Right m
             _ -> Left j)
-}
_JsonArray :: Prism' Json [Json]
_JsonArray =
  prism
    JsonArray
    (\j -> case j of
             JsonArray a -> Right a
             _ -> Left j)

setP ::
  Prism s t a b
  -> s
  -> Either t a
setP _ _ =
  error "todo: setP"

getP ::
  Prism s t a b
  -> b
  -> t
getP _ _ =
  error "todo: getP"

type Prism' a b =
  Prism a a b b

----

-- |
--
-- >>> modify fstL (+1) (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> modify sndL (+1) ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in modify fstL id (x, y) == (x, y)
--
-- prop> let types = (x :: Int, y :: String) in modify sndL id (x, y) == (x, y)
modify ::
  Lens s t a b
  -> (a -> b)
  -> s
  -> t
modify _ _ _ =
  error "todo: modify"

-- | An alias for @modify@.
(%~) ::
  Lens s t a b
  -> (a -> b)
  -> s
  -> t
(%~) =
  modify

infixr 4 %~

-- |
--
-- >>> fstL .~ 1 $ (0 :: Int, "abc")
-- (1,"abc")
--
-- >>> sndL .~ 1 $ ("abc", 0 :: Int)
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in set fstL (x, y) z == (fstL .~ z $ (x, y))
--
-- prop> let types = (x :: Int, y :: String) in set sndL (x, y) z == (sndL .~ z $ (x, y))
(.~) ::
  Lens s t a b
  -> b
  -> s
  -> t
(.~) _ _ _ =
  error "todo: (.~)"

infixl 5 .~

-- |
--
-- >>> fmodify fstL (+) (5 :: Int, "abc") 8
-- (13,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (10, "abc")
-- Just (20,"abc")
--
-- >>> fmodify fstL (\n -> bool Nothing (Just (n * 2)) (even n)) (11, "abc")
-- Nothing
fmodify ::
  Functor f =>
  Lens s t a b
  -> (a -> f b)
  -> s
  -> f t 
fmodify _ _ _ =
  error "todo: fmodify"

-- |
--
-- >>> fstL |= Just 3 $ (7, "abc")
-- Just (3,"abc")
--
-- >>> (fstL |= (+1) $ (3, "abc")) 17
-- (18,"abc")
(|=) ::
  Functor f =>
  Lens s t a b
  -> f b
  -> s
  -> f t
(|=) _ _ _ =
  error "todo: (|=)"

infixl 5 |=

-- |
--
-- >>> modify fstL (*10) (3, "abc")
-- (30,"abc")
-- in the lens library, _1
fstL ::
  Functor f => (a -> f b) -> (a, x) -> f (b, x)
  -- Lens (a, x) (b, x) a b
fstL k (a, x) = 
  (\b -> (b, x)) <$> k a

-- k :: a -> f b
-- a :: a
-- x :: x
-- ? :: f (b, x)

{-
type Lens s t a b =
  forall f.
  Functor f =>
  (a -> f b)
  -> s
  -> f t
-}

-- |
--
-- >>> modify sndL (++ "def") (13, "abc")
-- (13,"abcdef")
sndL ::
  Lens (x, a) (x, b) a b
sndL k (x,a) = -- (,) x <$> k a
  (,) x <$> k a

-- |
--
-- >>> get (mapL 3) (Map.fromList [(1,'a'),(3,'c')])
-- Just 'c'
--
-- >>> get (mapL 33) (Map.fromList [(1,'a'),(3,'c')])
-- Nothing
--
-- >>> set (mapL 3) (Map.fromList [(1,'a'),(3,'c')]) (Just 'X')
-- fromList [(1,'a'),(3,'X')]
--
-- >>> set (mapL 33) (Map.fromList [(1,'a'),(3,'c')]) (Just 'X')
-- fromList [(1,'a'),(3,'c'),(33,'X')]
--
-- Map.lookup :: Ord k => k -> Map k v -> Maybe v
-- Map.insert :: Ord k => k -> v -> Map k v -> Map k v
-- Map.delete :: Ord k => k -> Map k v -> Map k v

mapL ::
  Ord k =>
  k
  -> Lens (Map k v) (Map k v) (Maybe v) (Maybe v)
--(Functor f, Ord k) => k -> (Maybe v -> f (Maybe v)) -> Map k v -> f (Map k v)

mapL k p m = p (Map.lookup k m) <&> \x -> case x of
  Just v -> Map.insert k v m
  Nothing -> Map.delete k m 


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- |
--
-- >>> get (setL 3) (Set.fromList [1..5])
-- True
--
-- >>> get (setL 33) (Set.fromList [1..5])
-- False
--
-- >>> set (setL 3) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5]
--
-- >>> set (setL 3) (Set.fromList [1..5]) False
-- fromList [1,2,4,5]
--
-- >>> set (setL 33) (Set.fromList [1..5]) True
-- fromList [1,2,3,4,5,33]
--
-- >>> set (setL 33) (Set.fromList [1..5]) False
-- fromList [1,2,3,4,5]
setL ::
-- Set.member :: Ord k => k -> Set.Set k -> Bool
-- Set.insert :: Ord k => k -> Set.Set k -> Set.Set k
-- Set.delete :: Ord k => k -> Set.Set k -> Set.Set k
  Ord k =>
  k -> Lens (Set.Set k) (Set.Set k) Bool Bool
--(Ord k, Functor f) => k -> (Bool -> f Bool) -> Set.Set k -> f (Set.Set k)
setL k p s =
  --  Bool -> Set.Set k
-- (\b -> bool Set.delete Set.insert b k s) <$> (p (Set.member k s))
  (\b -> case b of True -> Set.insert k s
                   False -> Set.delete k s) <$> p (Set.member k s)

-- get (setL 4) (Set.fromList [1,2,3]) ==> False
-- get (setL 3) (Set.fromList [1,2,3]) ==> True
-- set (setL 4) True (Set.fromList [1,2,3]) ==> Set.fromList [1,2,3,4]
-- set (setL 4) False (Set.fromList [1,2,3]) ==> Set.fromList [1,2,3]
-- set (setL 3) True (Set.fromList [1,2,3]) ==> Set.fromList [1,2,3]
-- set (setL 3) False (Set.fromList [1,2,3]) ==> Set.fromList [1,2]
-- over (setL 4) not (Set.fromList [1]) ==> Set.fromList [1,4]
-- over (setL 4) not (Set.fromList [1,4]) ==> Set.fromList [1] 

{-
newtype Cont r a = Cont ((a -> r) -> r) deriving Functor

-- fmap f . fmap g = fmap (f . g)
-- fmap id = id


 -- (.) :: (b -> c) -> (a -> b) -> a -> c
class Contravariant f where
  -- contramap id = id
  -- contramap f . contramap g = contramap (g . f)
  contramap :: (a -> b) -> (f b -> f a)

class Profunctor p where
  -- dimap id f . dimap id g = dimap id (f . g)
  -- dimap f id . dimap g id = dimap (g . f) id
  -- dimap id id = id
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  -- dimap :: (a -> b) -> (c -> d) -> (->) b c -> (->) a d
  -- dimap :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
  dimap ab cd bc = cd . bc . ab


newtype Kleisli m a b = Kleisli (a -> m b)

data Predicate a = Predicate (a -> Bool)

type Lens s t a b = forall f. Functor f                    => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f           => (a -> f b) -> s -> f t
type Getter s a = forall f. (Functor f, Contravariant f)   => (a -> f a) -> s -> f s
type Fold s a = forall f. (Applicative f, Contravariant f) => (a -> f a) -> s -> f s


type Iso s t a b = forall p f. (Profunctor p, Functor f)   => p a (f b) -> p s (f t)

-- Lens s t a b = exists c. s -> (a, c), (b, c) -> t 

-- Lens' s a = exists c. s <-> (a, c)
-- Prism' s a = exists c. s <-> Either a c
-- Prism s t a b = exists c. (s -> Either a c, Either b c -> t)


lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt f s = sbt s <$> f (sa s)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)  



type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Iso s t a b = (s -> a, b -> t)

-}



-- |
--
-- >>> get (compose fstL sndL) ("abc", (7, "def"))
-- 7
--
-- >>> set (compose fstL sndL) ("abc", (7, "def")) 8
-- ("abc",(8,"def"))
compose ::
  Lens s t a b
  -> Lens q r s t
  -> Lens q r a b
compose _ _ =
  error "todo: compose"

-- | An alias for @compose@.
(|.) ::
  Lens s t a b
  -> Lens q r s t
  -> Lens q r a b
(|.) =
  compose

infixr 9 |.

-- |
--
-- >>> get identity 3
-- 3
--
-- >>> set identity 3 4
-- 4
identity ::
  Lens a b a b
identity =
  error "todo: identity"

-- |
--
-- >>> get (product fstL sndL) (("abc", 3), (4, "def"))
-- ("abc","def")
--
-- >>> set (product fstL sndL) (("abc", 3), (4, "def")) ("ghi", "jkl")
-- (("ghi",3),(4,"jkl"))
product ::
  Lens s t a b
  -> Lens q r c d
  -> Lens (s, q) (t, r) (a, c) (b, d)
product _ _ =
  error "todo: product"

-- | An alias for @product@.
(***) ::
  Lens s t a b
  -> Lens q r c d
  -> Lens (s, q) (t, r) (a, c) (b, d)
(***) =
  product

infixr 3 ***

-- |
--
-- >>> get (choice fstL sndL) (Left ("abc", 7))
-- "abc"
--
-- >>> get (choice fstL sndL) (Right ("abc", 7))
-- 7
--
-- >>> set (choice fstL sndL) (Left ("abc", 7)) "def"
-- Left ("def",7)
--
-- >>> set (choice fstL sndL) (Right ("abc", 7)) 8
-- Right ("abc",8)
choice ::
  Lens s t a b
  -> Lens q r a b
  -> Lens (Either s q) (Either t r) a b
choice _ _ =
  error "todo: choice"

-- | An alias for @choice@.
(|||) ::
  Lens s t a b
  -> Lens q r a b
  -> Lens (Either s q) (Either t r) a b
(|||) =
  choice

infixr 2 |||

----

type Lens' a b =
  Lens a a b b

cityL ::
  Lens' Locality String
cityL p (Locality c t y) =
  fmap (\c' -> Locality c' t y) (p c)

stateL ::
  Lens' Locality String
stateL p (Locality c t y) =
  fmap (\t' -> Locality c t' y) (p t)

countryL ::
  Lens' Locality String
countryL p (Locality c t y) =
  fmap (\y' -> Locality c t y') (p y)

streetL ::
  Lens' Address String
streetL p (Address t s l) =
  fmap (\t' -> Address t' s l) (p t)

suburbL ::
  Lens' Address String
suburbL p (Address t s l) =
  fmap (\s' -> Address t s' l) (p s)

localityL ::
  Lens' Address Locality
localityL p (Address t s l) =
  fmap (\l' -> Address t s l') (p l)

ageL ::
  Lens' Person Int
ageL p (Person a n d) =
  fmap (\a' -> Person a' n d) (p a)

nameL ::
  Lens' Person String
nameL p (Person a n d) =
  fmap (\n' -> Person a n' d) (p n)

addressL ::
  Lens' Person Address
addressL p (Person a n d) =
  fmap (\d' -> Person a n d') (p d)

intAndIntL ::
  Lens' (IntAnd a) Int
intAndIntL p (IntAnd n a) =
  fmap (\n' -> IntAnd n' a) (p n)

-- lens for polymorphic update
intAndL ::
  Lens (IntAnd a) (IntAnd b) a b
intAndL p (IntAnd n a) =
  fmap (\a' -> IntAnd n a') (p a)

-- |
--
-- >>> getSuburb fred
-- "Fredville"
--
-- >>> getSuburb mary
-- "Maryland"
getSuburb ::
  Person
  -> String
getSuburb =
  error "todo: getSuburb"

-- |
--
-- >>> setStreet fred "Some Other St"
-- Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setStreet mary "Some Other St"
-- Person 28 "Mary" (Address "Some Other St" "Maryland" (Locality "Mary Mary" "Western Mary" "Maristan"))
setStreet ::
  Person
  -> String
  -> Person
setStreet =
  error "todo: setStreet"

-- |
--
-- >>> getAgeAndCountry (fred, maryLocality)
-- (24,"Maristan")
--
-- >>> getAgeAndCountry (mary, fredLocality)
-- (28,"Fredalia")
getAgeAndCountry ::
  (Person, Locality)
  -> (Int, String)
getAgeAndCountry =
  error "todo: getAgeAndCountry"

-- |
--
-- >>> setCityAndLocality (fred, maryAddress) ("Some Other City", fredLocality)
-- (Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "Some Other City" "New South Fred" "Fredalia")),Address "83 Mary Ln" "Maryland" (Locality "Fredmania" "New South Fred" "Fredalia"))
--
-- >>> setCityAndLocality (mary, fredAddress) ("Some Other City", maryLocality)
-- (Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "Some Other City" "Western Mary" "Maristan")),Address "15 Fred St" "Fredville" (Locality "Mary Mary" "Western Mary" "Maristan"))
setCityAndLocality ::
  (Person, Address) -> (String, Locality) -> (Person, Address)
setCityAndLocality =
  error "todo: setCityAndLocality"
  
-- |
--
-- >>> getSuburbOrCity (Left maryAddress)
-- "Maryland"
--
-- >>> getSuburbOrCity (Right fredLocality)
-- "Fredmania"
getSuburbOrCity ::
  Either Address Locality
  -> String
getSuburbOrCity =
  error "todo: getSuburbOrCity"

-- |
--
-- >>> setStreetOrState (Right maryLocality) "Some Other State"
-- Right (Locality "Mary Mary" "Some Other State" "Maristan")
--
-- >>> setStreetOrState (Left fred) "Some Other St"
-- Left (Person 24 "Fred" (Address "Some Other St" "Fredville" (Locality "Fredmania" "New South Fred" "Fredalia")))
setStreetOrState ::
  Either Person Locality
  -> String
  -> Either Person Locality
setStreetOrState =
  error "todo: setStreetOrState"

-- |
--
-- >>> modifyCityUppercase fred
-- Person 24 "Fred" (Address "15 Fred St" "Fredville" (Locality "FREDMANIA" "New South Fred" "Fredalia"))
--
-- >>> modifyCityUppercase mary
-- Person 28 "Mary" (Address "83 Mary Ln" "Maryland" (Locality "MARY MARY" "Western Mary" "Maristan"))
modifyCityUppercase ::
  Person
  -> Person
modifyCityUppercase =
  error "todo: modifyCityUppercase"

-- |
--
-- >>> modifyIntAndLengthEven (IntAnd 10 "abc")
-- IntAnd 10 False
--
-- >>> modifyIntAndLengthEven (IntAnd 10 "abcd")
-- IntAnd 10 True
modifyIntAndLengthEven ::
  IntAnd [a]
  -> IntAnd Bool
modifyIntAndLengthEven =
  error "todo: modifyIntAndLengthEven"

----

-- |
--
-- >>> over traverseLocality (map toUpper) (Locality "abc" "def" "ghi")
-- Locality "ABC" "DEF" "GHI"
traverseLocality ::
  Traversal' Locality String
traverseLocality =
  error "todo: traverseLocality"

-- |
--
-- >>> over intOrIntP (*10) (IntOrIs 3)
-- IntOrIs 30
--
-- >>> over intOrIntP (*10) (IntOrIsNot "abc")
-- IntOrIsNot "abc"
intOrIntP ::
  Prism' (IntOr a) Int
intOrIntP =
  error "todo: intOrIntP"

intOrP ::
  Prism (IntOr a) (IntOr b) a b
intOrP =
  error "todo: intOrP"

-- |
--
-- >> over intOrP (even . length) (IntOrIsNot "abc")
-- IntOrIsNot False
--
-- >>> over intOrP (even . length) (IntOrIsNot "abcd")
-- IntOrIsNot True
--
-- >>> over intOrP (even . length) (IntOrIs 10)
-- IntOrIs 10
intOrLengthEven ::
  IntOr [a]
  -> IntOr Bool
intOrLengthEven =
  error "todo: intOrLengthEven"
