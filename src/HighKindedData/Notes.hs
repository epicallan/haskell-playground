{-# LANGUAGE TypeFamilies #-}

module HighKindedData.Notes where
import Universum

-- read http://reasonablypolymorphic.com/blog/higher-kinded-data/

--  "Higher-Kinded Data"

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data family PersonKD a

data Person' f = Person
 { pName :: HKD f String
 , pAge  :: HKD f Int
 } deriving (Generic)


type Person = Person' Identity

-- not that there is no need to wrap pName in Identity

validate :: Person' Maybe -> Maybe Person
validate (Person name age) = Person <$> name <*> age

maybePerson :: Person' Maybe
maybePerson = Person {pName = Just "allan",  pAge = Just 20}

