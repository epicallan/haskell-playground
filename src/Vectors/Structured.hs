{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Vectors.Structured where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Prelude hiding (replicate, (++))

{-
So, the (a?) problem with TypeNats from GHC is that it has no internal structure.
It’s basically the same as the Integer or Natural type — every single value (constructor)
is completely structurally unrelated to the next.

To add structure we use inductive type level nats
-}

$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

infixr 5 :+

data Vec (n :: Nat) (a :: Type) :: Type where
  VNil :: Vec 'Z a
  (:+) :: a -> Vec n a -> Vec ('S n) a

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f = \case
  VNil -> VNil
  (x :+ xs) -> f x :+ mapVec f xs

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec = \case
  VNil -> \case
    VNil -> VNil
  (x :+ xs) -> \case
    (y :+ ys) -> (x, y) :+ zipVec xs ys

type family (n :: Nat) + (m :: Nat) :: Nat where
    'Z   + m = m
    'S n + m = 'S (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = \case
    VNil    -> id
    x :+ xs -> \ys -> x :+ (xs ++ ys)


data Fin :: Nat -> Type where
    FZ :: Fin ('S n)  -- ^ we can create a 0th index if Vec n is non-empty
    FS :: Fin n       -- ^ if we have an ith index into a vector of size n
       -> Fin ('S n)  -- ... then we have an i+1th index into a vector of size ('S n)

deriving instance Show (Fin n)

-- | Fin ('S 'Z)

index :: Fin n -> Vec n a -> a
index = \case
    FZ -> \case
      x :+ _ -> x
    FS i -> \case
      _ :+ xs -> index i xs


{-
 data instance Sing (n :: Nat) where
   SZ :: Sing 'Z
   SS :: Sing n -> Sing ('S n)

-- Sing ('S 'S 'Z) = FS (FZ) -- Fin ('S ('S 'S 'Z))
-- Sing ('S 'S 'Z) = SS (SS SZ)

  class SingI k where
    sing :: Sing k

  Instance SingI 'Z where
   sing = SZ

  instance SingI ('S n) where
    sing = SS (sing  :: Sing n)
-}

replicate_ :: Sing n -> a -> Vec n a
replicate_ = \case
  SZ -> const VNil
  SS l -> \ x -> x :+ replicate_ l x

replicate :: SingI n => a -> Vec n a
replicate = replicate_ sing

generate_ :: Sing n -> (Fin n -> a) -> Vec n a
generate_ s f = case s of
  SZ   -> VNil
  SS l -> f FZ :+ generate_ l (f . FS)

generate :: SingI n => (Fin n -> a) -> Vec n a
generate = generate_ sing

withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r
withVec xs f = case xs of
  []     -> f SZ VNil
  x' : xs' -> withVec xs' $ \l ys ->
     f (SS l) (x' :+ ys)
{-

f SZ VNil $ \l ys -> f (SS l) (1 :+ ys)
-}

vecLength :: Vec n a -> Sing n
vecLength = \case
  VNil -> SZ
  _  :+ xs -> SS (vecLength xs)


-- exactLength :: Sing n -> Vec m a -> Maybe (Vec m a)
-- exactLength sn vm = case sn %~ vecLength vm of
--   Proved Refl -> Just vm
--   Disproved _ -> Nothing

exactLength_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLength_ sM v = case sM %~ vecLength v of
  Proved Refl -> Just v
  Disproved _ -> Nothing

exactLength :: SingI m => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing

exactLengthInductive_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLengthInductive_ = \case
    SZ -> \case
      VNil   -> Just VNil
      _ :+ _ -> Nothing
    SS l -> \case
      VNil    -> Nothing
      x :+ xs -> (x :+) <$> exactLengthInductive_ l xs

exactLengthInductive :: SingI m => Vec n a -> Maybe (Vec m a)
exactLengthInductive = exactLengthInductive_ sing

{-
 For example, we can make a witness that n is less than or equal to m,
 as well as a way to construct such a witness:
-}
data LTE :: Nat -> Nat -> Type where
  LEZ :: LTE 'Z m
  LES :: LTE n m -> LTE ('S n) ('S m)

-- incomplete patterns
isLTE :: Sing n -> Sing m -> Decision (LTE n m)
isLTE = \case
    SZ   -> \_ -> Proved LEZ
    SS n -> \case
      SZ -> Disproved $ \case {}       -- EmptyCase
      SS m -> case isLTE n m of
        Proved l    -> Proved $ LES l
        Disproved p -> Disproved $ \case
          LES l -> p l



atLeast_ :: Sing n -> Vec m a -> Maybe (LTE n m, Vec m a)
atLeast_ sN v = case isLTE sN (vecLength v) of
    Proved l    -> Just (l, v)
    Disproved _ -> Nothing

atLeast :: SingI n => Vec m a -> Maybe (LTE n m, Vec m a)
atLeast = atLeast_ sing


{-
We can write a function that can “take” an arbitrary amount from a vector,
given (via proof) that the vector has at least that many elements:
-}

takeVec :: LTE n m -> Vec m a -> Vec n a
takeVec = \case
    LEZ   -> const VNil
    LES l -> \case
      x :+ xs -> x :+ takeVec l xs

takeVecMaybe_ :: Sing n -> Vec m a -> Maybe (Vec n a)
takeVecMaybe_ sN v = uncurry takeVec <$> atLeast_ sN v

takeVecMaybe :: SingI n => Vec m a -> Maybe (Vec n a)
takeVecMaybe v = uncurry takeVec <$> atLeast v
