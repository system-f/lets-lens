{-# LANGUAGE RankNTypes #-}

module Lets.OpticPolyLens (
  Lens(..), tt1, tt2
, getsetLaw
, setgetLaw
, setsetLaw
, get
, set
, modify
, (%~)
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
, cityL
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
, modifyIntandLengthEven
) where

import Data.Char(toUpper)
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Set(Set)
import qualified Data.Set as Set(insert, delete, member)
import Lets.Data(AlongsideLeft(AlongsideLeft, getAlongsideLeft), AlongsideRight(AlongsideRight, getAlongsideRight), Identity(Identity, getIdentity), Const(Const, getConst), IntAnd(IntAnd), Person(Person), Locality(Locality), Address(Address), bool)
import Prelude hiding (product)

-- $setup
-- >>> import qualified Data.Map as Map(fromList)
-- >>> import qualified Data.Set as Set(fromList)
-- >>> import Data.Char(ord)
-- >>> import Lets.Data

data Lens s t a b =
  Lens
    (forall f. Functor f => (a -> f b) -> s -> f t)

-- data Const a b = Const { getConst :: a }
-- data Identity a = Identity a

get ::
  Lens s t a b
  -> s
  -> a
get (Lens r) =
  getConst . r Const

set ::
  Lens s t a b
  -> s
  -> b
  -> t
set (Lens r) a b =
  getIdentity (r (const (Identity b)) a)

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq s =>
  Lens s s a a
  -> s
  -> Bool
getsetLaw l =
  \a -> set l a (get l a) == a

-- | The set/get law of lenses. This function should always return @True@.
setgetLaw ::
  Eq a =>
  Lens s s a a
  -> s
  -> a
  -> Bool
setgetLaw l a b =
  get l (set l a b) == b

-- | The set/set law of lenses. This function should always return @True@.
setsetLaw ::
  Eq s =>
  Lens s s a b
  -> s
  -> b
  -> b
  -> Bool 
setsetLaw l a b1 b2 =
  set l (set l a b1) b2 == set l a b2

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
-- Functor f => (a -> f b) -> s -> f t
  Lens s t a b -> (a -> b) -> s -> t
modify =
  \(Lens l) -> \f -> \s -> getIdentity (l (\a -> Identity (f a)) s)

-- l :: (a -> f b) -> s -> f t
-- f :: a -> b
-- s :: s
-- ? :: t

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
(.~) =
--  \l b -> modify l (\_ -> b)
-- \l b -> modify l (const b)
-- \l b -> (modify l . const) b
  \l -> modify l . const

-- \x -> f (g x)
-- f . g

--   \(Lens l) -> \b -> \s -> getIdentity (l (\_ -> Identity b) s)
--   \(Lens l) -> \f -> \s -> getIdentity (l (\a -> Identity (f a)) s)

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
fmodify =
  \(Lens l) -> l

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
(|=) =
-- \l b -> fmodify l (\_ -> b)
  \l -> fmodify l . const

infixl 5 |=

-- |
--
-- >>> modify fstL (*10) (3, "abc")
-- (30,"abc")
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw fstL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw fstL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw fstL (x, y) z
fstL ::
  Lens (a, x) (b, x) a b
fstL =
--Lens (\a2fb -> \(a, x) -> (\fb -> fmap (\b -> (b, x)) fb) (a2fb a))
  Lens (\a2fb -> \(a, x) -> fmap (\b -> (b, x)) (a2fb a))

data Persoon = Persoon Int String

nameLens :: Lens Persoon Persoon String String
nameLens = 
  Lens (\f (Persoon age name) -> fmap (\name' -> Persoon age name') (f name))

-- (\x -> f x) a
-- f a

-- |
--
-- >>> modify sndL (++ "def") (13, "abc")
-- (13,"abcdef")
--
-- prop> let types = (x :: Int, y :: String) in getsetLaw sndL (x, y)
--
-- prop> let types = (x :: Int, y :: String) in setgetLaw sndL (x, y) z
--
-- prop> let types = (x :: Int, y :: String) in setsetLaw sndL (x, y) z
sndL ::
  Lens (x, a) (x, b) a b
sndL =
  Lens (\a2fb -> \(x, a) -> fmap (\b -> (x, b)) (a2fb a))

-- |
--
-- >>> get (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Just 'c'
--
-- >>> get (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d']))
-- Nothing
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'X'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) (Just 'X')
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(33,'X')]
--
-- >>> set (mapL 3) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(4,'d')]
--
-- >>> set (mapL 33) (Map.fromList (map (\c -> (ord c - 96, c)) ['a'..'d'])) Nothing
-- fromList [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
mapL ::
  Ord k =>
  k
  -> Lens (Map k v) (Map k v) (Maybe v) (Maybe v)
mapL =
  \k -> Lens (\f -> \m -> fmap (\may -> case may of
    Nothing -> Map.delete k m
    Just v -> Map.insert k v m) (f (Map.lookup k m)))

-- f :: (Maybe v -> f (Maybe v))
-- m :: Map k v

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
  Ord k =>
  k
  -> Lens (Set k) (Set k) Bool Bool
setL =
-- \k -> Lens (\f s -> fmap (\b -> bool (Set.delete k s) (Set.insert k s) b) (f (Set.member k s)))
-- \k -> Lens (\f s -> fmap (bool (Set.delete k s) (Set.insert k s)) (f (Set.member k s)))
-- \k -> Lens (\f s -> fmap (\b -> bool (Set.delete k s) (Set.insert k s) b) (f (Set.member k s)))
  \k -> Lens (\f s -> fmap (\b -> bool Set.delete Set.insert b k s) (f (Set.member k s)))

-- if p then t else f
-- case of
-- bool f t p

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
compose =
  \(Lens l1) (Lens l2) -> Lens (l2 . l1)

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
  Lens id

tt1 :: Lens a a a a 
tt1 = Lens id

-- LAWS!
tt2 :: Lens a a a a
tt2 = Lens (\f a -> fmap (const a) (f a))

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
product =
  \(Lens l1) (Lens l2) ->
    Lens (\f (s, q) ->
      getAlongsideRight (l2 (\x2 ->
        AlongsideRight (getAlongsideLeft (l1 (\x1 ->
          AlongsideLeft (f (x1, x2))) s))) q))

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
choice =
  \(Lens l1) (Lens l2) ->
    Lens (\f e -> case e of
      Left s -> fmap Left (l1 f s)
      Right q -> fmap Right (l2 f q))

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
cityL =
  Lens
    (\p (Locality c t y) -> fmap (\c' -> Locality c' t y) (p c))

stateL ::
  Lens' Locality String
stateL =
  Lens
    (\p (Locality c t y) -> fmap (\t' -> Locality c t' y) (p t))

countryL ::
  Lens' Locality String
countryL =
  Lens
    (\p (Locality c t y) -> fmap (\y' -> Locality c t y') (p y))

streetL ::
  Lens' Address String
streetL =
  Lens
    (\p (Address t s l) -> fmap (\t' -> Address t' s l) (p t))

suburbL ::
  Lens' Address String
suburbL =
  Lens
    (\p (Address t s l) -> fmap (\s' -> Address t s' l) (p s))

localityL ::
  Lens' Address Locality
localityL =
  Lens
    (\p (Address t s l) -> fmap (\l' -> Address t s l') (p l))

ageL ::
  Lens' Person Int
ageL =
  Lens
    (\p (Person a n d) -> fmap (\a' -> Person a' n d) (p a))

nameL ::
  Lens' Person String
nameL =
  Lens
    (\p (Person a n d) -> fmap (\n' -> Person a n' d) (p n))

addressL ::
  Lens' Person Address
addressL =
  Lens
    (\p (Person a n d) -> fmap (\d' -> Person a n d') (p d))

intAndIntL ::
  Lens' (IntAnd a) Int
intAndIntL =
  Lens
    (\p (IntAnd n a) -> fmap (\n' -> IntAnd n' a) (p n))

-- lens for polymorphic update
intAndL ::
  Lens (IntAnd a) (IntAnd b) a b
intAndL =
  Lens
    (\p (IntAnd n a) -> fmap (\a' -> IntAnd n a') (p a))

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
  get (suburbL |. addressL)


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
  get (suburbL ||| cityL)

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
  set (streetL |. addressL ||| stateL)

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
  cityL |. localityL |. addressL %~ map toUpper

-- |
--
-- >>> modify intAndL (even . length) (IntAnd 10 "abc")
-- IntAnd 10 False
--
-- >>> modify intAndL (even . length) (IntAnd 10 "abcd")
-- IntAnd 10 True
modifyIntandLengthEven ::
  IntAnd [a]
  -> IntAnd Bool
modifyIntandLengthEven =
  intAndL %~ even . length
