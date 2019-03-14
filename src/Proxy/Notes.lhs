> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE RankNTypes #-}

> module Proxy.Notes where
> import Data.Tagged
> import Data.Proxy

This notes are largely refrenced from https://kseo.github.io/posts/2017-01-15-data-proxy.html

Proxy is a poly kinded proxy type

Kind of proxy is

< :K Proxy
< Proxy :: k -> *

This means k is poly kinded and we can pass any type to Proxy.

< Proxy Char means k is *
< Proxy (,) means k is * -> *

we can create any value of kind * by using undefined eg

< let b = undefined :: Int

but we cant do the same for

< let q = undefined :: (,)

with Proxy we can overcome this

< let q = Proxy :: Proxy (,)

Applications

Solve type ambiguity

> f :: forall a. (Read a, Show a) => Proxy a -> String -> String
> f _ = (show :: a -> String) . read

< f (Proxy :: Proxy Int) "3"

can also be solved by TypeApplications

< f :: forall a. (Read a, Show a) => String -> String
< f s = show (read @Int s)
