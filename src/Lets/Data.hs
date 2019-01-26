module Lets.Data (
  Locality(..)
, Address(..)
, Person(..)
, IntAnd(..)
, IntOr(..)
, fredLocality
, fredAddress
, fred
, maryLocality
, maryAddress
, mary
, Store(..)
, Const (..)
, Tagged(..)
, Identity(..)
, AlongsideLeft(..)
, AlongsideRight(..)
) where

import Control.Applicative(Applicative(..))
import Data.Monoid(Monoid(..))

data Locality =
  Locality
    String -- city
    String -- state
    String -- country
  deriving (Eq, Show)  

data Address =
  Address
    String -- street
    String -- suburb
    Locality
  deriving (Eq, Show)  

data Person =
  Person
    Int -- age
    String -- name
    Address -- address
  deriving (Eq, Show)

data IntAnd a =
  IntAnd
    Int
    a
  deriving (Eq, Show)

data IntOr a =
  IntOrIs Int
  | IntOrIsNot a
  deriving (Eq, Show)

fredLocality ::
  Locality
fredLocality =
  Locality
    "Fredmania"
    "New South Fred"
    "Fredalia"

fredAddress ::
  Address
fredAddress =
  Address
    "15 Fred St"
    "Fredville"
    fredLocality

fred ::
  Person
fred =
  Person
    24
    "Fred"
    fredAddress

maryLocality ::
  Locality
maryLocality =
  Locality
    "Mary Mary"
    "Western Mary"
    "Maristan"

maryAddress ::
  Address
maryAddress =
  Address
    "83 Mary Ln"
    "Maryland"
    maryLocality

mary ::
  Person
mary =
  Person
    28
    "Mary"
    maryAddress

----

data Store s a =
  Store
    (s -> a)
    s

data Const a b =
  Const {
    getConst :: 
      a
  }
  deriving (Eq, Show)

instance Functor (Const a) where
  fmap _ (Const a) =
    Const a

instance Monoid a => Applicative (Const a) where
  pure _ =
    Const mempty
  Const f <*> Const a =
    Const (f `mappend` a)

data Tagged a b = 
  Tagged {
    getTagged ::
      b
  }
  deriving (Eq, Show)

instance Functor (Tagged a) where
  fmap f (Tagged b) =
    Tagged (f b)

instance Applicative (Tagged a) where
  pure =
    Tagged
  Tagged f <*> Tagged a =
    Tagged (f a)

data Identity a =
  Identity {
    getIdentity ::
      a
  }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) =
    Identity (f a)

instance Applicative Identity where
  pure =
    Identity
  Identity f <*> Identity a =
    Identity (f a)

data AlongsideLeft f b a =
  AlongsideLeft {
    getAlongsideLeft :: 
      f (a, b)
  }

instance Functor f => Functor (AlongsideLeft f b) where
  fmap f (AlongsideLeft x) =
    AlongsideLeft (fmap (\(a, b) -> (f a, b)) x)

data AlongsideRight f a b =
  AlongsideRight {
    getAlongsideRight ::
      f (a, b)
  }

instance Functor f => Functor (AlongsideRight f a) where
  fmap f (AlongsideRight x) =
    AlongsideRight (fmap (\(a, b) -> (a, f b)) x)
