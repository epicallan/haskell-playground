> {-# LANGUAGE TypeFamilies  #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE PolyKinds #-}
> {-#LANGUAGE GADTs #-}


> module TypeFamilies.Background where
> -- import GHC.TypeLits

TypeFamilies background

Essentially, type families allow us to write functions on types.


Like type synonyms, type families can’t be partially applied (“unsaturated”).
They only ever make sense in “fully applied” (or “saturated”) form, with all arguments given syntactically.

````
  data Z
  data S n

  type family Plus m n :: *
  type instance Plus Z n = n
  type instance Plus (S m) n = S (Plus m n)
```

This says that for any types m and n, Plus m n is type of kind *.
But it isn’t a new type, it’s just an alias for some existing type

It’s instructive to think carefully about the difference between this and type synonyms

We can define some type foo as below

``` type Foo m n = [(m, Maybe n)] ````

which defines the type synonym Foo uniformly for all arguments m and n,
but using only type synonyms we cannot say

```
  type Foo m Int = [m]
  type Foo m Char = Maybe m
````


Total type families
__________________

A type family whose equations cover all the possible cases and which is guaranteed to terminate.
That is, a total type family is properly a function on types.


Non-covering type families
______________________________________

A non-covering type family is a type family whose patterns do not cover the whole space.
Let’s consider closed and open families separately.

A closed type Family

> type family F1 a where
>   F1 Int = Bool

> sillyId :: F1 Char -> F1 Char
> sillyId x = x

Open type families
________________________

> type family F2 a
> type instance F2 Int = Bool

A data  family is  a  family  of  datatype  definitions.
Each  instance in the family has its own set of data constructors, but the family shares one type constructor.
The applicable data constructors for a particular datatype are determined by the parameters to the datafamily.

example

> data family Sing (a :: k)

> data Nat where
>  Zero :: Nat
>  Succ :: Nat -> Nat

> data instance Sing (a :: Bool) where
>   STrue :: Sing 'True
>   SFalse :: Sing 'False

> data instance Sing (a :: Nat) where
>   SZero :: Sing 'Zero
>   SSucc :: forall (n :: Nat). Sing n -> Sing ('Succ n)
