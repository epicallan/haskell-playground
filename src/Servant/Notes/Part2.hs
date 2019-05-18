{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Servant.Notes.Part2 where

import Data.Kind (Type)
import Data.List (intercalate)

-- chain a few "endpoint components" with this operator,
-- all chains must be terminated with a 'Verb' component.
infixr 5 :>
data (a :: Type) :> (b :: Type) = a :> b

data Method = Get | Post

newtype Verb = Verb Method

data Capture a = Capture

newtype Static = Static String

-- a class to specify all the valid endpoint descriptions

class EndPoint a

instance EndPoint Verb

instance EndPoint rest => EndPoint (Static :> rest)

instance EndPoint rest => EndPoint (Capture a :> rest)

-- GET /hello

endpoint1 :: Static :> Verb
endpoint1 = Static "hello" :> Verb Get

type Link = [String]

-- @renderLink ["hello", "world"] == "/hello/world"@
renderLink :: Link -> String
renderLink xs = '/' : intercalate "/" xs

class HasLink endpoint where
  type LinkType endpoint :: Type
  link :: endpoint -> [String] -> LinkType endpoint

instance HasLink Verb where
  type LinkType Verb = Link
  link _ = reverse

instance HasLink api => HasLink (Static :> api) where
  type LinkType (Static :> api) = LinkType api
  link (Static str :> api) acc = link api (str : acc)

instance (Show a, HasLink api) => HasLink (Capture a :> api) where
  type LinkType (Capture a :> api) = a -> LinkType api
  link (Capture :> api) acc captureValue = link api (show captureValue : acc)

endpoint2 :: Static :> Capture Int :> Verb
endpoint2 = Static "hello" :> Capture @Int :> Verb Post
