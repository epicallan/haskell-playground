>  {-# LANGUAGE RankNTypes #-}
> module RankN.Notes where

TODO: go through https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html

RankNTypes allow us to enforce parametric polymorhism explicitly.
consider

> tupleF :: (a -> b) -> (a, a) -> (b, b)
> tupleF elemF (x, y) = (elemF x, elemF y)

we can do

< tupleF length ([1, 2], [2, 3, 5])

but we cant do

< tupleF show (True, [2, 3, 5])

to get this working we need to make tupleF to take a polymorphic function

> tupleF' :: (Show a1,  Show a2)
>         => (forall a . Show a => a -> b)
>         -> (a1, a2)
>         -> (b, b)
> tupleF' elemF (x, y) = (elemF x, elemF y)

< tupleF' show (True, [2, 3, 5])


> main = do
>   print $ tupleF' show (True, [2, 3, 5])
>   print $ tupleF' show (1, [2, 3, 5])


Rank of a type refers to nexting depth at which polymorhism occusrs

< intAdd :: Int -> Int -> Int

is Rank 0

and tupleF' is Rank 2 while tupleF is Rank 1

> applyToBoth :: (forall a. a -> a) -> (Int, Bool)
> applyToBoth f = (f 5, f True)

In the function below forall a is associated to  brokenApplyToBoth.
Caller of the function gets to chose type of `a`

< brokenApplyToBoth :: forall a. (a -> a) -> (Int, Bool)
< brokenApplyToBoth' f = (f 5, f True)
