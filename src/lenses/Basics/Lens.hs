module Lenses.Basics.Lens where

import Control.Applicative
import Data.Functor.Identity

{-
 type Lens s a = forall f. Function => (a -> f s) -> s -> f s
-}

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

type LensGetter s a = (a -> Const a a) -> s -> Const a s

type LensModify s a = (a -> Identity a) -> s -> Identity s

view_ :: LensGetter s a -> s -> a
view_ l = getConst . l Const

view :: Lens s a -> s -> a
view l = getConst . l Const

over_ :: LensModify s a -> (a -> a) -> s -> s
over_ l f = runIdentity . l (Identity . f)

over :: Lens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

set :: Lens s a -> a -> s -> s
set l x = runIdentity . l (\_ -> Identity x)

newtype Address = Address
  { addressCity   :: String
  }

data Person = Person
  { name :: String
  , age  :: Int
  -- , address :: Address
  } deriving (Eq, Read, Show)

_name :: Lens Person String
_name f Person{..} =  (`Person` age) <$> f name

_age :: Lens Person Int
_age f Person{..} = (name `Person`) <$> f age

person :: Person
person = Person
  { name = "allan"
  , age =  28
  }

viewName :: String
viewName = view _name person

setName :: Person
setName = set _name "allan" person

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter f s = setter s <$> f (getter s)

namel :: Lens Person String
namel = lens nameGetter nameSetter
  where
    nameGetter :: Person -> String
    nameGetter Person {..} = name

    nameSetter :: Person -> String -> Person
    nameSetter p newName = p {name = newName}

viewNamel :: String
viewNamel = view namel person

-- | Composition

data PersonWithAddress = PersonWithAddress
  { address :: Address
  , job     :: String
  }

personWithAddress :: PersonWithAddress
personWithAddress = PersonWithAddress
  { address = Address { addressCity = "kampala" }
  , job = "doctor"
  }
-- | address city lens
 -- f (g x) = f . g
-- addressCityl :: (String -> f String) -> Address -> f Address
addressCityl :: Lens Address String
addressCityl = lens cityGetter citySetter
  where
    cityGetter :: Address -> String
    cityGetter Address {..} = addressCity

    citySetter :: Address -> String -> Address
    citySetter add newCity = add { addressCity = newCity }

-- personAddressL :: (Address -> f Address) -> PersonWithAddress -> f PersonWithAddress
personAddressl :: Lens PersonWithAddress Address
personAddressl = lens addrGetter addrSetter
  where
    addrGetter :: PersonWithAddress -> Address
    addrGetter PersonWithAddress {..} = address

    addrSetter :: PersonWithAddress -> Address -> PersonWithAddress
    addrSetter pa newAddr = pa { address = newAddr }


-- | lens composition
personCityAddressL
  :: Functor f
  => (String -> f String) -> PersonWithAddress -> f PersonWithAddress
personCityAddressL = personAddressl . addressCityl

viewAddress :: String
viewAddress = view personCityAddressL personWithAddress

-- | Polymorphic lens type

type Lens' s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- the old monomorphic variant
type Lens_ s a = Lens' s s a a

-- say we have a data type with a poly value

data PersonPoly age = PersonPoly
  { personPolyName :: String
  , personPolyAge  :: age
  }


lens_ :: (s -> a) -> (s -> b -> t) -> Lens' s t a b
lens_ getter setter f s = setter s <$> f (getter s)

personAgepl :: Lens' (PersonPoly age1) (PersonPoly age2) age1 age2
personAgepl = lens_ personPolyAge personAgeplSetter
  where
    personAgeplSetter :: PersonPoly age1 -> age2 -> PersonPoly age2
    personAgeplSetter PersonPoly{..} age2 =  PersonPoly
      { personPolyName = personPolyName
      ,  personPolyAge = age2
      }
