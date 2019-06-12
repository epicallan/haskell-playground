{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module DependentHaskell.Vectors where

-- import Prelude hiding (tail, head)

{-|
Reference: https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell

Dependent types among other things safe guard code against boundary condition violation.

bottom :: Int
bottom = head [] accessing the head of empty list!

A common solution to boundary code violation is to parameterize the length of the lists in its type.
ie.

```
[1,2,3] :: Vector Int 3

head :: Vector a (n+1) -> a
```

Vector type seems to depend on the value 0, n+1 and so on, and such types depending on values are called dependent types.

-}


data Nat = Z | S Nat

infixl 6 :+
-- infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance 'Z   :+ m = m
type instance 'S n :+ m = 'S (n :+ m)

-- TODO: implement multiplication

data Vector a n where
  Nil  :: Vector a 'Z
  (:-) :: a -> Vector a n -> Vector a ('S n)

infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

-- TODO: implement head, tail and append

-- main :: IO ()
-- main = do
--   print $ head (1 :- 2 :- Nil)
--   print $ tail (1 :- 2 :- Nil)
  -- | Uncommenting the line below causes type error
  -- print $ head Nil
-- /show
