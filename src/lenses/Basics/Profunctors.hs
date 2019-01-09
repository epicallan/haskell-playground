{-# LANGUAGE RankNTypes #-}

module Lenses.Basics.Profunctors where

{-
lmap :: Profunctor p => (a -> b) -> p b c -> p a c

rmap :: Profunctor p => (b -> c) -> p a b -> p a c

dimap :: Profunctor p => (a -> b) -> (c -> d) -> p b c -> p a d

The point of profunctors is that if you're given a p a b, you can treat it as an opaque “black box”,
    some kind of relationship between a and b – you can add a filter to the black box
    which would modify its output, and you can add another filter which would modify its input,
    but you can't modify the black box itself in any way and you can't inspect the input
    in any way (because, after all, there might not even be any)

What's useful about profunctors isn't that you can use dimap to operate on
something that happens to be a profunctor,
but that you can write functions which work on several profunctors
-}

