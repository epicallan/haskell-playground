> {-# LANGUAGE TypeFamilies          #-}

TypeFamilies or GADTs extension are required to work with EqualityConstraint

> module EqualityConstraint.Notes where

- (~) represents EqualityConstraint operator. It denotes that 2 types need to be the same. 


Examples

function:

< sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2

> class PairOf a b where
>   thePair :: (a, b)

< instance Monoid a => PairOf' a a where
<   thePair = (mempty, mempty)


< obtuseMempty :: Monoid t => t
< obtuseMempty = fst thePair

The obtuseMempty function would error with below error 

 Could not deduce (PairOf a b0) arising from a use of ‘thePair’
  from the context: Monoid a
    bound by the type signature for:
               obtuseMempty :: Monoid a => a
    at sample.hs:11:1-29
  The type variable ‘b0’ is ambiguous


solution is to use EqualityConstraint

> instance (Monoid a, a ~ b) => PairOf a b where
>   thePair = (mempty, mempty)

> obtuseMempty :: Monoid t => t
> obtuseMempty = fst thePair