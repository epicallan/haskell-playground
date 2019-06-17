-- | code from http://hackage.haskell.org/package/vector-sized
-- Using TypeNats to create sized vectors
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Vectors.Nats where

import Data.Finite
import Data.Proxy
import Data.Type.Equality
import qualified Data.Vector as V
import GHC.TypeNats

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a}

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == len = Just (UnsafeMkVec v)
        | otherwise = Nothing
  where
    len = fromIntegral $ natVal (Proxy @n)

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f v = UnsafeMkVec $ V.map f (getVector v)

-- just for fun


instance Functor (Vec n) where
    fmap = mapVec

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(UnsafeMkVec xs) ++ (UnsafeMkVec ys) = UnsafeMkVec (xs V.++ ys)

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zip xs ys)

takeVec :: forall n m a. KnownNat n =>  Vec (n + m) a -> Vec n a
takeVec (UnsafeMkVec xs) = UnsafeMkVec $ V.take size xs
  where
    size = fromIntegral $ natVal (Proxy @n)

splitVec :: forall n m a. KnownNat n => Vec (n + m) a -> (Vec n a, Vec m a)
splitVec (UnsafeMkVec xs) = (UnsafeMkVec ys, UnsafeMkVec zs)
  where
    l = fromIntegral (natVal (Proxy @n))
    (ys, zs) = V.splitAt l xs

{-
  We can use a Finite n to “index” a Vector n a.
  A Vector n a has exactly n slots, and a Finite n has n possible values.
  Clearly, Finite n only contains valid indices into our vector
  index will never fail at runtime due to a bad index — do you see why?
  Valid indices of a Vector 5 a are the integers 0 to 4, and that is precisely
  the exact things that Finite 5 can store!

-}

index :: Vec n a -> Finite n -> a
index v i = getVector v V.! fromIntegral (getFinite i)

replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (natVal (Proxy @n))

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (natVal (Proxy @n))

 -- | Converting from unsized vector to sized vector
{-
  data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)

  someNatVal :: Natural -> SomeNat
-}

withVec :: V.Vector a -> (forall n. KnownNat n => Vec n a -> r) -> r
withVec v f = case someNatVal (fromIntegral (V.length v)) of
  SomeNat (Proxy :: Proxy m) -> f (UnsafeMkVec @m v)

getThird :: V.Vector a -> Maybe a
getThird v = withVec v $ \v' -> fmap (v' `index`) (packFinite 2)

-- | ensuring 2 vectors have the same length

{-
data (:~:) :: k -> k -> Type where
    Refl :: x :~: x

sameNat
    :: (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> Maybe (n :~: m)
-}

exactLength :: forall n m a. (KnownNat n, KnownNat m) => Vec n a -> Maybe (Vec m a)
exactLength v = case sameNat (Proxy @n) (Proxy @m) of
  Just Refl -> Just v
  Nothing   -> Nothing


zipSame :: forall a b. V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
zipSame v1 v2 = withVec v1 $ \(v1' :: Vec n a) ->
                withVec v2 $ \(v2' :: Vec m b) ->
                  case exactLength @n @m v1' of
                    Just v1Same -> Just $ getVector (zipVec v1Same v2') -- v1' has the same length as v2'
                    Nothing     -> Nothing
