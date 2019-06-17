-- | code from http://hackage.haskell.org/package/vector-sized
-- Using Singletons to create a sized vector
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module Vectors.Sings where

import Data.Finite
import Data.Proxy
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.TypeLits
import qualified Data.Vector as V
import GHC.TypeNats

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a}

-- implicit sing style
mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == len = Just (UnsafeMkVec v)
        | otherwise = Nothing
  where
    len = fromIntegral (fromSing (sing :: Sing n))

-- explicit Sing style
mkVec_ :: forall n a. Sing n -> V.Vector a -> Maybe (Vec n a)
mkVec_ s v | V.length v == l = Just (UnsafeMkVec v)
           | otherwise = Nothing
  where
    l = fromIntegral $ fromSing s


-- alternatively, re-using `mkVec_`
mkVec__ :: KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec__ = mkVec_ sing

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

replicate_ :: Sing n -> a -> Vec n a
replicate_ s x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral $ fromSing s

replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (natVal (Proxy @n))


 -- | Converting from unsized vector to sized vector
{-
  data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)

  someNatVal :: Natural -> SomeNat
-}

withVec_ :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec_ v f = case toSing (fromIntegral (V.length v)) of
  SomeSing s -> f s (UnsafeMkVec v)

withVec :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec v f = withSomeSing (fromIntegral (V.length v)) $ \s -> f s (UnsafeMkVec v)

-- getThird :: V.Vector a -> Maybe a
-- getThird v = withVec v $ \v' -> fmap (v' `index`) (packFinite 2)

-- | ensuring 2 vectors have the same length

{-
data (:~:) :: k -> k -> Type where
    Refl :: x :~: x
x
sameNat
    :: (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> Maybe (n :~: m)
-}

exactLength_ :: Sing m -> Sing n -> Vec n a -> Maybe (Vec m a)
exactLength_ sM sN v = case sM %~ sN of
  Proved Refl -> Just v
  Disproved _ -> Nothing

exactLength :: (KnownNat m, KnownNat n) => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing sing

-- zipSame :: forall a b. V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
-- zipSame v1 v2 = withVec v1 $ \(v1' :: Vec n a) ->
--                 withVec v2 $ \(v2' :: Vec m b) ->
--                   case exactLength @n @m v1' of
--                     Just v1Same -> Just $ getVector (zipVec v1Same v2') -- v1' has the same length as v2'
--                     Nothing     -> Nothing

{-
-- SNat can be used to construct a `Sing` if we have a `KnownNat` constraint
-- It can also be pattern matched on to reveal a `KnownNat constraint`
SNat :: KnownNat n => Sing n

-- we can give a `Sing n` and be able to execute something in the context where
-- that `n` has a `KnownNat` constraint
withKnownNat :: Sing n -> (KnownNat n => r) -> r
-}

generate_ :: Sing n -> (Finite n -> a) -> Vec n a
generate_ s f = withKnownNat s $
    UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

-- alternatively, via pattern matching:

generate'_ :: Sing n -> (Finite n -> a) -> Vec n a
generate'_ s@SNat f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

generate :: KnownNat n => (Finite n -> a) -> Vec n a
generate = generate_ sing
