{-# LANGUAGE TypeFamilies #-}
-- | examples from ghc wiki
module TypeFamiliesWiki where
import qualified Data.IntMap as IntMap
-- list like data family
data family XList a

-- list like instance for char
data instance XList Char = XCons Char (XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit Int

-- | associtated data type families

class GMapKey k where
    data GMap k :: * -> *
    empty       :: GMap k v
    lookup      :: k -> GMap k v -> Maybe v
    insert      :: k -> v -> GMap k v -> GMap k v

-- Int instance

instance GMapKey Int where
    data GMap Int v = GMapInt (IntMap.IntMap v)
    empty = GMapInt Data.IntMap.empty
    lookup k   (GMapInt m) = IntMap.lookup k m
    insert k v (GMapInt m) = GMapInt (IntMap.insert k v m)

