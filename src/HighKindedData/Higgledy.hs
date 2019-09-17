{-# LANGUAGE NoImplicitParams #-}
module HighKindedData.Higgledy where

-- import Universum as Prelude

-- | exploring  making of higgledy lib

{-
the idea seems to be to make a monoid out of any structure where functor f is applied to the
fields of the structure
-}

data User = User
  { name :: String
  , age  :: Int
  }

-- say we want to apply a functor Identity to each field of User
-- so as to make it high-kinded
-- generics are used for this.

-- newtype HKD structure f = HKd { runHkd :: HKD_ f structure }

-- HKD_ constructs
