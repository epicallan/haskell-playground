> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-#LANGUAGE GADTs #-}

> module DataKinds.Notes where 

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