{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Servant.TinyServant.Notes where

import Control.Applicative
import Data.Kind
import Data.Proxy
import Data.Time
import GHC.TypeLits
import Text.Read

-- API specifications

data Get (a :: Type)

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: Type)
infixr 9 :>

data Capture (a :: Type)

{-

A Get a represents an endpoint of type a (of kind *). "person" :> Get Person

With a :<|> b, we represent the choice between two routes.

Capture Currency :> Get Amount

With item :> rest, we represent nested routes, where item is the first path component
and rest are the remaining components. In our simplified DSL, there are just two possibilities
for item: a type-level string, or a Capture.
-}

-- Example API

type MyAPI =
           "date" :> Get Day
      :<|> "time" :> Capture TimeZone :> Get ZonedTime

{-
 The API type can be interpreted in many ways by servant, our interpretation will follow
 the serve function interpretation

  serve :: HasServer layout
      => Proxy layout -> Server layout -> Application

  The Application type that come from WAI library can be assumed to be of type;

  [String] -> IO String

  where [String] is a sequence of path components

  serve :: HasServer layout => Proxy layout _> Server layout -> [String] -> IO String

  * Server is a type family
  * HasServer is a class with instance for API types
-}

-- open type family for extensibility

type family Server layout :: Type

type instance Server (Get a) = IO a

type instance Server (a :<|> b) = Server a :<|> Server b

-- literal strings in the route don't affect the type of the handler
type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

{-
 If we expand Server MyAPI, we obtain

 Server MyAPI
 ~ Server ( "date :> Get Day
            :<|> "time" :> Capture TimeZone :> Get ZonedTime
          )
 ~ IO Day :<|> TimeZone -> IO ZonedTime
-}

-- defining handler functions

handleDate :: IO Day
handleDate = utctDay <$> getCurrentTime

handleTime :: TimeZone -> IO ZonedTime
handleTime tz = utcToZonedTime tz <$> getCurrentTime

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime

class HasServer layout where
  route :: Proxy layout -> Server layout -> [String] -> Maybe (IO String)

-- defining serve in terms of route

serve :: HasServer layout
      => Proxy layout
      -> Server layout
      -> [String]
      -> IO String
serve px handler paths = case route px handler paths of
  Nothing -> ioError (userError "404")
  Just m  -> m

-- if none of the routes match we fail with a simulated 404

-- HasServer instances

-- for type instance Server (Get a) = IO a

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a)
        -> IO a -> [String] -> Maybe (IO String)
  route _ handler [] = Just (show <$> handler)
  route _ _ _        = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b)
        -> (Server a :<|> Server b) -> [String] -> Maybe (IO String)
  route _ (aHandler :<|> bHandler) paths =
        route (Proxy :: Proxy a) aHandler paths
    <|> route (Proxy :: Proxy b) bHandler paths

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Proxy (s :> r)
        -> Server r -> [String] -> Maybe (IO String)
  route _ (handler) (x : xs) |
    symbolVal (Proxy :: Proxy s) == x = route (Proxy :: Proxy r) handler xs
  route _ _ _                         = Nothing

-- test
runServer :: HasServer MyAPI => IO String
runServer = serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "CET"]

{-
GHCi> serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "CET"]
"2015-11-01 20:25:04.594003 CET"
GHCi> serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "12"]
*** Exception: user error (404)
GHCi> serve (Proxy :: Proxy MyAPI) handleMyAPI ["date"]
"2015-11-01"
GHCi> serve (Proxy :: Proxy MyAPI) handleMyAPI []
*** Exception: user error (404)
-}