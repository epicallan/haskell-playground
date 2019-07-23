{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ImplicitParams.Notes where

import Data.Proxy
import GHC.Classes
import GHC.TypeLits

{-
 -- | conventional usage
foo :: (?x :: Int) => Int
foo = ?x

-- | binding x to a value using let binding
bar :: Int
bar = foo

-- | x can as well be defined globally
instance IP "x" Int where
  ip = 10

-}

-- | Global implicit variables by defining instances of IP class
instance KnownSymbol symbol => IP symbol String where
  ip = symbolVal (Proxy @symbol)

bye :: String
bye = ?thanks <> " " <> ?for <> " " <> ?reading
