-- | Length Indexed list

-- {-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}

module GADTs.HList where

data HList (ts :: [*] ) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hlistHead :: HList (t ': ts) -> t
hlistHead (t :# _) = t


showBool :: HList '[_1, Bool, _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs
