{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | examples from ghc wiki
module TypeFamilies.WikiEx where

import Prelude hiding (lookup)

import Data.Kind (Type)

import qualified Data.IntMap as IntMap

-- list like data family
data family XList a

-- list like instance for char
data instance XList Char = XCons Char (XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit Int

-- | associtated data type families

class GMapKey k where
    data GMap k :: Type -> Type
    empty       :: GMap k v
    lookup      :: k -> GMap k v -> Maybe v
    insert      :: k -> v -> GMap k v -> GMap k v

-- Int instance

instance GMapKey Int where
    data GMap Int v = GMapInt (IntMap.IntMap v)
    empty = GMapInt IntMap.empty
    lookup k   (GMapInt m) = IntMap.lookup k m
    insert k v (GMapInt m) = GMapInt (IntMap.insert k v m)


printVal :: (GMapKey a, Show b) => a -> GMap a b -> String
printVal x gMap = show $ lookup x gMap

gMap' :: GMap Int String
gMap' = insert 1 "first-key" empty

printEff :: IO ()
printEff = putStrLn $ printVal 1 gMap'

-- From paper

type family AddF m n
type instance AddF Int Float = Int
type instance AddF Float Float = Int

addF :: Int -> Float -> AddF Int Float
addF x y = round $ fromIntegral x + y
