{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels       #-}
module OverloadedLabels.Notes where
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits

-- | Manual records access, this can be simplified with overloaded record fields
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
  fromLabel x = getValue x (Get :: Label s)

-- example

getX :: Int
getX = #x (Point 1 2)

-- #x (Point 1 2)
