> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-#LANGUAGE GADTs #-}
> {-# LANGUAGE PolyKinds #-}

> module DataKinds.Notes where
> import Data.Kind

Notes on Data kind

In my own words; data kinds extension allows one to define types of a data type constructors.

The extension provides a simple mechanism called promotion to populate the kind level of a data constructor.
This may also mean that data constructors of a data type become constructors that can be constrained to particular
types.

Another definiton / explanation for datakinds extension is (http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html);

The DataKinds extension allows us to promote data constructors into type constructors, which also promotes their type constructors into kind constructors.
To promote something up a level, we prefix the name with an apostrophe, or tick: '.


For instance a length indexed vector can be defined this way at type level

< data vector :: * -> * -> *

with data kinds type promotion, vector kind level parameters can be constrained


> data Nat = Zero | Succ Nat

> data Vector (n :: Nat) (a :: *) where
>   VNil :: Vector 'Zero a  -- A value constructed by VNil can have any type a, but the length is always constrained to be 'Zero
>   VCons :: a -> Vector n a -> Vector ('Succ n) a -- The inductive case is adding another value to a vector. One more value means one more length.

Note that without DataKinds extention you have this kind of definition at type level.
Because `Nat` construcors wouldn't be type constructors

< data Vector :: * -> * -> * where
<  VNil :: Vector Nat a
<  VCons :: a -> Vector n a -> Vector Nat a

Kind polymorhism

Proxy is by default supposed to take in a type of kind * meaning it wont work for all types.

:k Proxy

Proxy :: Type -> Type

without PolyKind extension to enble kind polymorhisphm one would resort to such declarations

> data Proxy1 a = MkProxy1
> data Proxy2 (a :: Type -> Type) = MkProxy2

used as below

> stringRep = MkProxy1 :: Proxy1 String
> maybeRep  = MkProxy2 :: Proxy2 Maybe

with PolyKinds this is solved as below

> stringRep' = MkProxy1 :: Proxy1 String
> maybeRep' = MkProxy1 :: Proxy1 Maybe