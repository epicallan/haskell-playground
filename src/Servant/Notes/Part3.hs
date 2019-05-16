{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
module Servant.Notes.Part3 where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits

data Static (str :: Symbol)

data Capture (a :: Type)

data Method = Get | Post

data Verb (method :: Method)

infixr 5 :>

data (a :: Type) :> (b :: Type)

type Link = [String]
{-
we will use data Proxy a = Proxy to act as an intermediate between the value level,
where the calls to link will happen, and the type level, where the descriptions live
and drive the link interpretation through our typeclass instances.
-}

link :: HasLink api => Proxy api -> LinkType api
link api = link' api []

class HasLink api where
  type LinkType api :: Type
  link' :: Proxy api -> Link -> LinkType api

instance HasLink (Verb method) where
  type LinkType (Verb method) = Link
  link' _ = reverse

instance (KnownSymbol str, HasLink api) => HasLink (Static str :> api) where
  type LinkType (Static str :> api) = LinkType api
  link' _ acc = link' (Proxy @api) (symbolVal (Proxy @str) : acc)

instance (Show a, HasLink api) => HasLink (Capture a :> api) where
  type LinkType (Capture a :> api) = a -> LinkType api
  link' _ acc param = link' (Proxy @api) (show param : acc)


type GetHello = Static "hello" :> Capture Int :> Capture Double :> Verb 'Get

linkHello :: Int -> Double -> Link
linkHello = link (Proxy :: Proxy GetHello)

link1 :: Link
link1 = linkHello 40 0.1
