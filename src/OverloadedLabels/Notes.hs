{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels       #-}
module OverloadedLabels.Notes where
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits

-- | GHC Docs example
data Label (l :: Symbol) = Get

class KnownSymbol l => GetValue r l a | r l -> a where
  getValue :: r -> Label l -> a

data Point = Point
  { x :: Int
  , y :: Int
  }

instance GetValue Point "x" Int  where
  getValue (Point x _) _ = x

instance GetValue Point "y" Int where
  getValue (Point _ y) _ = y

instance GetValue Point s a => IsLabel (s :: Symbol) (Point -> a) where
  fromLabel point = getValue point (Get :: Label s)

{-
 A call to a label gets translated into  fromlabel :: IsLabel s a => a
-}

-- example

getX :: Int
getX = #x (Point 1 2)

-- #x (Point 1 2)

