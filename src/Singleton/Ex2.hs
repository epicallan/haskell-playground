{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType      #-}
module Singleton.Ex2 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

data Door :: DoorState -> Type where
  UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data OldSomeDoor :: Type where
  OldSomeDoor :: DoorState -> String -> OldSomeDoor

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkDoor :: Sing s -> String -> Door s
mkDoor _  = UnsafeMkDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor dsing door) =
  let state = fromSing dsing
  in OldSomeDoor state (doorMaterial door)

fromOld :: OldSomeDoor -> SomeDoor
fromOld (OldSomeDoor ds material) = case toSing ds of
  SomeSing s -> fromDoor s $ mkDoor s material

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
    | n `mod` 2 == 1 = Just (UnsafeMkDoor m)
    | otherwise      = Nothing

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n door = case unlockDoor n door of
  Just closedDoor -> fromDoor SClosed closedDoor
  Nothing         -> fromDoor SLocked door

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor material) = UnsafeMkDoor material

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n somedoor@(MkSomeDoor s door) = withSingI s $
  case openAnyDoor n door of
     Just x  -> fromDoor_ x
     Nothing -> somedoor

data HList (ts :: [Type]) :: Type where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hListVals :: HList '[Int, Bool]
hListVals =  2 :# True :# HNil

infixr 5 :>
data List a = Nil | a :> (List a)

data instance Sing :: List a -> Type where
  SNil :: Sing 'Nil
  SCons :: Sing x -> Sing xs -> Sing (x ':> xs )

-- SFalse : STrue  : SNill
singList ::Sing ('Opened ':> ('Closed ':> 'Nil))
singList = SCons SOpened  (SClosed `SCons` SNil)

instance SingKind k => SingKind (List k) where
  type Demote (List k) = List (Demote k)

  fromSing :: Sing (xs :: List k) -> List (Demote k)
  fromSing SNil         = Nil
  fromSing (SCons x xs) = fromSing x :> fromSing xs

  toSing :: List (Demote k) -> SomeSing (List k)
  toSing Nil       = SomeSing SNil
  toSing (x :> xs) =
    withSomeSing x (\sx -> withSomeSing xs (SomeSing . SCons sx) )

instance SingI 'Nil where
  sing :: Sing 'Nil
  sing = SNil

instance (SingI a, SingI b) => SingI (a ':> b) where
  sing :: Sing (a ':> b)
  sing = SCons (sing :: Sing a) (sing :: Sing b)


data IsTrue :: Bool -> Type where
  IsTrueable :: IsTrue 'True

someFn ::  IsTrue b -> Maybe Int
someFn _ = Just 1

someStaff :: Maybe Int
someStaff = someFn IsTrueable
