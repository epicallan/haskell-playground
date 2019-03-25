> {-# LANGUAGE  ExistentialQuantification #-}
> {-# LANGUAGE  ScopedTypeVariables #-}
> module Existential.Notes where

An existentially quantified type is one that is hidden to the user/consumer but directly
chosen by the produced.

eg

 myDoor :: SomeDoor

where

data SomeDoor = forall s. MkSomeDoor (Sing s) (Door s)

A universally quantified type (which is the norm) is one where the type is directly chosen
by the user.

eg

read :: Read a => String -> a

read is universally quantified over a: The caller of read gets to pick which type is given.

Closely related to rank-n types are existential types. E
xistentials are types which, for one reason or another,
the type-system doesn’t know anything about.

Existential Quantified types is away of squashing types into one.
Think usage of forall in function definitions.

< map :: forall a b . (a -> b) -> [a] -> [b]

Example of an Existential data types

> data T = forall a . Show a => MKT a

The interaction of the universal quantifier with data types produces a qualified subset of types
guaranteeing certain facilities as described by one or more class constraints.

The problem with broken is that the type b in apply is not the same b in broken.
Haskell thinks it knows better than us here, and introduces a new type variable for apply


< broken :: (a -> b) -> a -> b
< broken f a = apply
<   where
<     apply :: b
<     apply = f a


When scopedTypeVariables is enabled, we will be able to bind and refer to type
variables later on.

The forall a b. quantifier exposes the type variables a and b to the remainder of the function’s definition.
This allows us to reuse b when adding the type signature to apply,
rather than introducing a new type variable.

> working :: forall a b. (a -> b) -> a -> b
> working f a = apply
>   where
>       apply :: b
>       apply = f a