{-# LANGUAGE RankNTypes #-}

module Lenses.Basics.Iso where

{-
“Iso” is a shortening of “isomorphism”; an Iso' s a:

    is a lens which lets you access a in s
    is a lens which lets you access s in a (when inverted)
    is isomorphic to (s -> a, a -> s)

So, if you have an Iso' s a, it means that you can convert between s and a without losing any information.
-}

{-

An Iso s t a b is a generalisation that also lets you change the types:

Iso s t a b ~ (s -> a, b -> t)

    over someIso        :: (a -> b) -> (s -> t)
    over (from someIso) :: (t -> s) -> (b -> a)
    view someIso        :: s -> a
    view (from someIso) :: b -> t
-}

-- If we want isos to work in both directions,
-- we just need some type of “bidirectional function” which could work in either direction.
-- Then constructing and inverting isos would be trivial:

-- type Iso s t a b = forall f. Functor f => (a -> f b) <-> (s -> f t)

class Isomorphic k where
    isomorphic :: (a -> b) -> (b -> a) -> k a b

data Isomorphism a b = Isomorphism (a -> b) (b -> a)

instance Isomorphic Isomorphism where
    isomorphic = Isomorphism

-- reversing an iso

-- from :: Isomorphism a b -> Isomorphism b a
-- from :: Isomorphism a b -> (b -> a)

from :: Isomorphic k => Isomorphism a b -> k b a
from (Isomorphism a b) = isomorphic b a

type Iso s t a b =
    forall k f. (Isomorphic k, Functor f) =>
    k (a -> f b) (s -> f t)

-- And you can create an iso from functions the easy way –
-- just make 2 lenses going in opposite directions:

isos
    :: (s -> a) -> (a -> s) -- s <-> a
    -> (t -> b) -> (b -> t) -- t <-> b
    -> Iso s t a b
isos sa as tb bt = isomorphic
    (\afb s -> bt <$> afb (sa s))
    (\sft a -> tb <$> sft (as a))


