> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-#LANGUAGE GADTs #-}

> module DataKinds.BuiltInTypes where
> import GHC.TypeLits

The promoted version of a String is called a SYMBOL.
SYMBOLs are not lists of characters—they are completely distinct things

Eg

< :kind "hello"
< "hello" :: Symbol

Natural numbers are of Kind Nat

< :kind 5085072209
< 5085072209 :: Nat

Lists are promoted as below given a below list definition

< data [a] = [] | a : [a]

< '[] of kind [A]
< '(:) of kind a -> [a]

Make sure to add a space after a promoted list

< :kind '[ 'True ]
<'[ 'True ] :: [Bool]

Safe Money example while using PhantomTypes

> newtype Money (currency :: Symbol) = Money Rational

> twoEuros :: Money "Euro"
> twoEuros = Money 2

if we had the currency at term level we would have to do runtime checks and every function
would have to signal the possibilty of failure by returning Maybe, Either or MonadError

> addMoney :: Money c -> Money c -> Money c
> addMoney (Money x) (Money y) = Money (x + y)

Safemoney also represents scales as Nat, for instance cents would have a scale of 100 to 1

> newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat)) = Discrete Integer

> oneDollar :: Discrete "USD" '(1, 1)
> oneDollar = Discrete 1

> oneDollarThirtyCents :: Discrete "USD" '(100, 1)
> oneDollarThirtyCents = Discrete 130
