-- | Notes from thinking with types
module Generic.Notes where

import Data.Kind
{-
 Background
___________

Remember that all types can be experessed as sums of products i.e they can all be built
from Either or Tupels
-}

data Maybe_ a = Nothing_ | Just_ a

toCananical :: Maybe_ a -> Either () a
toCananical = \case
  Just_ x -> Right x
  Nothing_ -> Left ()

fromCananical :: Either () a -> Maybe_ a
fromCananical = \case
  Right x -> Just_ x
  Left () -> Nothing_
{-
 Given the ability to convert to and from cananical representation we can have the tools for
 structural polymorphism

 The generics extensions enables derivation of generic class instances for custom classes
-}

class Generic a where
  type Rep a :: Type -> Type
  from :: a -> Rep a x
  to :: Rep a x -> a

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