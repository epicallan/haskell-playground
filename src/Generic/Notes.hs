-- | Notes from thinking with types
module Generic.Notes where

import Data.Kind
import GHC.Generics
{-
 Background
___________

Remember that all types can be experessed as sums of products i.e they can all be built
from Either or Tupels
-}

data Maybe_ a = Nothing_ | Just_ a

toCanonical :: Maybe_ a -> Either () a
toCanonical = \case
  Just_ x -> Right x
  Nothing_ -> Left ()

fromCanonical :: Either () a -> Maybe_ a
fromCanonical = \case
  Right x -> Just_ x
  Left () -> Nothing_
{-
 Given the ability to convert to and from cananical representation we can have the tools for
 structural polymorphism

 The generics extensions enables derivation of generic class instances for custom classes
-}

class Generic_ a where
  type Rep_ a :: Type -> Type
  from_ :: a -> Rep_ a x
  to_ :: Rep_ a x -> a

{-
 A Bool class instance would result in below types
 Rep Bool = ( ... UI :+:  ... UI)
 :+: is analogous to | in data constructors and because True and False have no data UI are used
 to represent Unit type

 > :kind! Rep Bool
   Rep Bool :: Type -> Type =
   D1 ('MetaData "Bool" "GHC.Types" "ghc-prim" 'False)

   ( C1 ('MetaCons "False" 'PrefixI 'False) U1
    :+:
    C1 ('MetaCons "True" 'PrefixI 'False) U1
   )
-}

-- | Rep examples

data Empty

{-

newtype M1 (i :: Type) (c :: Meta) (f :: k -> Type) (p :: k)

type D1 = M1 D -- (c f p)

data V1 (p :: k)

type V1 :: k -> Type - stands for Void (Kind constructor)

data Meta =

  MetaData Symbol Symbol Symbol Bool
   -- ^ MetaData n m p nt
   -- n data type name
   -- m module in which data type is defined
   -- p package in which data type is defined
   -- nt is 'True if datatype is newtype

  MetaCons Symbol FixityI Bool
  -- ^ MetaCons n f s
  -- n is the constructors name
  -- f is its fixity
  -- s is 'True if the constructor contains record selector

  MetaSel (Maybe Symbol) (SourceUnpackedness) (SourceStrictness) (DecidedStrictness)

-}
instance Generic Empty where
  type Rep Empty =
    D1 ('MetaData "Empty" "Generic.Notes" "Generic.Notes" 'False) V1

  from = error "do me"
  to = error "do me"

data Boolean = True_ | False_

{-
 type C1 = M1 C
 data U1 (p :: k) -- stands for unit type
 type UI :: k -> Type -- kind constructor

-- sum type encoding
 data (f :: k -> Type) :+: (g :: k -> Type)) (p :: k) = L1 (f p) | R1 (g p)
-}

instance Generic Boolean where
  type Rep Boolean =
    D1 ('MetaData "Boolean" "Data.Bool" "package-name" 'False)
      ( C1 ('MetaCons "False" 'PrefixI 'False) U1 :+:
        C1 ('MetaCons "True" 'PrefixI 'True ) U1
      )
  -- ^ U1 stands for unit type

  from = error "do me"
  to = error "do me"

{-

-- product type encoding
data (f :: k -> Type) :*: (g :: k -> Type)) (p :: k)

-- R is a type level proxy that has no associated values
type Rec0 = K1 R

newtype K1 (i :: Type) c  (p :: k) = K1 { unk1 :: c }

-}
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Generic (Tree a) where
  type Rep (Tree a) =
    D1 ('MetaData "Tree" "Main" "package-name" 'False)
      (C1 ('MetaCons "Leaf" 'PrefixI 'False)
         (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
                (Rec0 a))
       :+:
       C1 ('MetaCons "Node" 'PrefixI 'False)
         (S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
               (Rec0 (Tree a))
          :*:
          S1 ('MetaSel 'Nothing
                         'NoSourceUnpackedness
                         'NoSourceStrictness
                         'DecidedLazy)
               (Rec0 (Tree a))))

  from = error "do me"
  to = error "do me"

{--
 reduced implementation
instance Generic (Tree a) where
  type Rep (Tree a) =
    Rec0 a
    :+:
    (Rec0 (Tree a) :*: Rec0 (Tree a))

-}

{-
-- Definitions of the generic representation types

data    V1        p                       -- lifted version of Empty
data    U1        p = U1                  -- lifted version of ()
data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

-}

-- | Generically deriving Eq class



{-
 Approach to generically derive sturctural polymorphism is
threefold
 1 . Define a typeclass to act as a carrier
 2 . Provide inductive instances of the class for the generic constructors
 3 . Finally write a helper function to map between the Rep and the desired type
-}

class GEq (a :: k1 -> Type) where
  geq :: a x -> a x -> Bool

instance GEq U1 where
  geq U1 U1 = True

instance GEq V1 where
  geq _ _ = True

instance Eq a => GEq  (K1 _i a ) where
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _             = False


instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2


instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

--  genericEq True False

infixr 5 <<=

class GOrd (a :: k -> Type) where
  (<<=) :: a x -> a x -> Bool

instance GOrd U1 where U1 <<= U1 = False

instance GOrd V1 where _ <<= _ = False

instance Ord a => GOrd (K1 _i a) where
  (K1 a) <<= (K1 b) = a <= b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  L1 a1 <<= L1 a2 = a1 <<= a2
  R1 b1 <<= R1 b2 = b1 <<= b2
  L1 _ <<= _      = True -- TODO: cross check
  R1 _ <<= _      = False


instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  (a1 :*: b1) <<= (a2 :*: b2) = a1 <<= a2 && b1 <<= b2

instance GOrd a => GOrd (M1 _x _y a) where
  (M1 a1) <<= (M1 a2) = a1 <<= a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Bool
genericOrd a b = from a <<= from b
