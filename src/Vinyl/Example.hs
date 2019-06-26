{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Vinyl.Example where

import Control.Lens hiding (Identity)
import Data.Singletons.TH (genSingletons)
import Data.Vinyl


data Fields = Name | Age | Sleeping | Master deriving Show

-- Any record can be now described by a type-level list of these labels.
-- The @DataKinds@ extension must be enabled to autmatically turn all the
-- constructors of the @Field@ type into types.

type LifeForm = ['Name, 'Age, 'Sleeping]

-- Now, we need a way to map our labels to concrete types. We use a type
-- family for this purpose.
-- Unfortunately, type families aren't first class in Haskell.  That's
-- why we also need a data type, with which we will parametrise 'Rec'.
-- We also generate the necessary singletons for each field label using
-- Template Haskell.


type family ElF (f :: Fields) :: * where
    ElF 'Name = String
    ElF 'Age = Int
    ElF 'Sleeping = Bool
    ElF 'Master = Rec Attr LifeForm

newtype Attr f = Attr { _unAttr :: ElF f }

makeLenses ''Attr
genSingletons [ ''Fields ]

instance Show (Attr 'Name) where show (Attr x) = "name: " ++ show x
instance Show (Attr 'Age) where show (Attr x) = "age: " ++ show x
instance Show (Attr 'Sleeping) where show (Attr x) = "sleeping: " ++ show x
instance Show (Attr 'Master) where show (Attr x) = "master: " ++ show x


-- To make field construction easier, we define an operator.  The first
-- argument of this operator is a singleton - a constructor bringing the
-- data-kinded field label type into the data level.  It's needed because
-- there can be multiple labels with the same field type, so by just
-- supplying a value of type @ElF f@ there would be no way to deduce the
-- correct "f".


-- (=::) :: sing f -> ElF f -> Attr f

-- _ =:: x = Attr x

(=::) :: sing f -> ElF f -> Attr f
_ =:: x = Attr x

type Person = ['Name, 'Age, 'Sleeping]

jon :: Rec Attr Person
jon = SName =:: "jon"
        :& (SAge =:: 23)
        :& (SSleeping =:: False)
        :& RNil


-- accessor
-- name ::
