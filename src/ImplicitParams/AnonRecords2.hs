{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE ImplicitParams         #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
module ImplicitParams.AnonRecords2 where

import Data.Kind
import GHC.Classes
import GHC.TypeLits
import Unsafe.Coerce

-- | original https://gitlab.com/snippets/1874588
-- Anonymous records with implicitParams

data Rec :: [Constraint] -> Type where
  Rec :: All fields => Rec fields

type family All (cs :: [Constraint]) = (c :: Constraint) | c -> cs where
  All '[] = ()
  All (c ': cs) = (c, All cs)

empty :: Rec '[]
empty = Rec

-- | examples
type Person = [?name :: String, ?age :: Int]

person :: Rec Person
person = Rec where
  ?name = "Allan"
  ?age  = 28

-- | adding new field

-- | Helper for field addition without use of binding and annotation.
add :: forall s t fs. t -> Rec fs -> Rec (IP s t ': fs)
add v Rec = reify @s v Rec -- can use reflection package

reify :: forall s t r. t -> (IP s t => r) -> r
reify v x = unsafeCoerce (Want @s @t @r x) v

newtype Want s t r where
  Want :: (IP s t => r) -> Want s t r

-- | add usage example
personWithJob :: Rec (IP "job" String ': Person)
personWithJob = add "Doctor" person

type family TypeOf (s :: Symbol) (fs :: [Constraint]) :: Type where
  TypeOf s (IP s t ': fs) = t
  TypeOf s ( _ ': fs)     = TypeOf s fs
  TypeOf s '[]            =
    TypeError ('Text "Missing field '" ':<>: 'Text s ':<>: 'Text " '")

class Has (s :: Symbol) (cs :: [Constraint]) where
  get :: Rec cs -> TypeOf s cs
  set :: TypeOf s cs -> Rec cs -> Rec cs

instance Has s '[] where
  get = error "get: missing field instance"
  set = error "set: missing field instance"

instance Has s cs => Has s (c ': cs) where
  get :: Rec (c ': cs) -> TypeOf s (c ': cs)
  get Rec = get @s (Rec :: Rec cs)

  set v Rec = extend $ set @s @cs v Rec

instance {-# Overlapping #-} Has s (IP s t : cs) where
  get Rec = ip @s @t
  set  v = add @s v . shrink

extend :: c => Rec cs -> Rec (c ': cs)
extend Rec = Rec

shrink :: Rec (c ': cs) -> Rec cs
shrink Rec = Rec

-- | examples with Has class

getName :: String
getName = get @"name" person

getName_ :: Has "name" cs => Rec cs -> TypeOf "name" cs
getName_ rec = get @"name" rec

myName :: String
myName = getName_ person

setName :: Rec Person
setName = set @"name" "alex" person

-- TODO: implement merging of constraints
-- TODO: implement Removal of a constraint
