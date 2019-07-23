{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

-- | Anonymous records with ImplicitParams
-- original https://gist.github.com/llelf/6c01ded225877914f38a

module ImplicitParams.AnonRecords1 where

import Data.Kind (Constraint)

data Rec (fields :: Constraint) where
  Rec :: fields => Rec fields

infixr 5 ?:

(?:) :: Rec fields -> (fields => r) -> r
Rec ?: r = r

type Person = (?name :: String, ?age :: Int)

-- | you can as well use where
record :: Rec Person
record =
  let ?name = "allan"
      ?age = 28
  in Rec

getAge :: Int -- | ()
getAge = record ?: ?age

getName :: String
getName = record ?: ?name
