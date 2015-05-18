module Lets.GetSetLens (
  Lens(..)
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
, getSuburb
, setStreet
, getAgeAndCountry
, setCityAndLocality
, getSuburbOrCity
, setStreetOrState
, modifyCityUppercase
) where

import Control.Applicative((<*>))
import Data.Bool(bool)
import Data.Char(toUpper)
import Data.Map(Map)
import qualified Data.Map as Map(insert, delete, lookup)
import Data.Set(Set)
import qualified Data.Set as Set(insert, delete, member)
import Lets.Data
import Prelude hiding (product)

-- $setup
-- >>> import qualified Data.Map as Map(fromList)
-- >>> import qualified Data.Set as Set(fromList)
-- >>> import Data.Char(ord)

data Lens a b =
  Lens
    (a -> b -> a)
    (a -> b)

-- |
--
-- >>> get fstL (0 :: Int, "abc")
-- 0
--
-- >>> get sndL ("abc", 0 :: Int)
-- 0
--
-- prop> let types = (x :: Int, y :: String) in get fstL (x, y) == x
--
-- prop> let types = (x :: Int, y :: String) in get sndL (x, y) == y
get ::
  Lens a b
  -> a
  -> b
get (Lens _ g) =
  g

-- |
--
-- >>> set fstL (0 :: Int, "abc") 1
-- (1,"abc")
--
-- >>> set sndL ("abc", 0 :: Int) 1
-- ("abc",1)
--
-- prop> let types = (x :: Int, y :: String) in set fstL (x, y) z == (z, y)
--
-- prop> let types = (x :: Int, y :: String) in set sndL (x, y) z == (x, z)
set ::
  Lens a b
  -> a 
  -> b
  -> a
set (Lens s _) a =
  s a

-- | The get/set law of lenses. This function should always return @True@.
getsetLaw ::
  Eq a =>
  Lens a b
  -> a
  -> Bool
getsetLaw l =
  \a -> set l a (get l a) == a
  
-- | The set/get law of lenses. This function should always return @True@.
setgetLaw ::
  Eq b =>
  Lens a b
  -> a
  -> b
  -> Bool
setgetLaw l a b =
  get l (set l a b) == b
  
-- | The set/set law of lenses. This function should always return @True@.
setsetLaw ::
  Eq a =>
  Lens a b
  -> a
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
  Lens a b
  -> (b -> b)
  -> a
  -> a
modify (Lens s g) f a =
  s a (f (g a))

-- | An alias for @modify@.
(%~) ::
  Lens a b
  -> (b -> b)
  -> a
  -> a
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
  Lens a b
  -> b
  -> a
  -> a
(.~) l =
  modify l . const

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
  Lens a b
  -> (b -> f b)
  -> a
  -> f a
fmodify (Lens s g) f a =
  fmap (s a) (f (g a))

-- |
--
-- >>> fstL |= Just 3 $ (7, "abc")
-- Just (3,"abc")
--
-- >>> (fstL |= (+1) $ (3, "abc")) 17
-- (18,"abc")
(|=) ::
  Functor f =>
  Lens a b
  -> f b
  -> a
  -> f a
(|=) l =
  fmodify l . const

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
  Lens (x, y) x
fstL =
  Lens
    (\(_, y) x -> (x, y))
    (\(x, _) -> x)

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
  Lens (x, y) y
sndL =
  Lens
    (\(x, _) y -> (x, y))
    (\(_, y) -> y)

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
  -> Lens (Map k v) (Maybe v)
mapL k =
  Lens
    (maybe . Map.delete k <*> (flip (Map.insert k)))
    (Map.lookup k)

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
  -> Lens (Set k) Bool
setL k =
  Lens
    (bool . Set.delete k <*> Set.insert k)
    (Set.member k)

-- |
--
-- >>> get (compose fstL sndL) ("abc", (7, "def"))
-- 7
--
-- >>> set (compose fstL sndL) ("abc", (7, "def")) 8
-- ("abc",(8,"def"))
compose ::
  Lens b c
  -> Lens a b
  -> Lens a c
compose (Lens s1 g1) (Lens s2 g2) =
  Lens
    (\a -> s2 a . s1 (g2 a))
    (g1 . g2)

-- | An alias for @compose@.
(|.) ::
  Lens b c
  -> Lens a b
  -> Lens a c
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
  Lens a a
identity =
  Lens
    (const id)
    id

-- |
--
-- >>> get (product fstL sndL) (("abc", 3), (4, "def"))
-- ("abc","def")
--
-- >>> set (product fstL sndL) (("abc", 3), (4, "def")) ("ghi", "jkl")
-- (("ghi",3),(4,"jkl"))
product ::
  Lens a b
  -> Lens c d
  -> Lens (a, c) (b, d)
product (Lens s1 g1) (Lens s2 g2) =
  Lens
    (\(a, c) (b, d) -> (s1 a b, s2 c d))
    (\(a, c) -> (g1 a, g2 c))

-- | An alias for @product@.
(***) ::
  Lens a b
  -> Lens c d
  -> Lens (a, c) (b, d)
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
  Lens a x
  -> Lens b x
  -> Lens (Either a b) x
choice (Lens s1 g1) (Lens s2 g2) =
  Lens
    (\e x -> either (\a -> Left (s1 a x)) (\b -> Right (s2 b x)) e)
    (either g1 g2)

-- | An alias for @choice@.
(|||) ::
  Lens a x
  -> Lens b x
  -> Lens (Either a b) x
(|||) =
  choice

infixr 2 |||

----

cityL ::
  Lens Locality String
cityL =
  Lens
    (\(Locality _ t y) c -> Locality c t y)
    (\(Locality c _ _) -> c)

stateL ::
  Lens Locality String
stateL =
  Lens
    (\(Locality c _ y) t -> Locality c t y)
    (\(Locality _ t _) -> t)

countryL ::
  Lens Locality String
countryL =
  Lens
    (\(Locality c t _) y -> Locality c t y)
    (\(Locality _ _ y) -> y)

streetL ::
  Lens Address String
streetL =
  Lens
    (\(Address _ s l) t -> Address t s l)
    (\(Address t _ _) -> t)

suburbL ::
  Lens Address String
suburbL =
  Lens
    (\(Address t _ l) s -> Address t s l)
    (\(Address _ s _) -> s)

localityL ::
  Lens Address Locality
localityL =
  Lens
    (\(Address t s _) l -> Address t s l)
    (\(Address _ _ l) -> l)

ageL ::
  Lens Person Int
ageL =
  Lens
    (\(Person _ n d) a -> Person a n d)
    (\(Person a _ _) -> a)

nameL ::
  Lens Person String
nameL =
  Lens
    (\(Person a _ d) n -> Person a n d)
    (\(Person _ n _) -> n)

addressL ::
  Lens Person Address
addressL =
  Lens
    (\(Person a n _) d -> Person a n d)
    (\(Person _ _ d) -> d)

-- |
--
-- >>> get (suburbL |. addressL) fred
-- "Fredville"
--
-- >>> get (suburbL |. addressL) mary
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
  set (streetL |. addressL)

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
  get (ageL *** countryL)

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
  set (cityL |. localityL |. addressL *** localityL)
  
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
