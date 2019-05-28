{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType      #-}
module Singleton.Ex1 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

data Door :: DoorState -> Type where
  UnsafeMkDoor :: { doorMaterial :: String } -> Door s

{-
genSingletons  generates;
data Sing :: DoorState -> Type
  SOpened :: Sing 'Opened
  etc

instance SingI 'Opened where
  sing = SOpened

withSingI :: Sing s -> (for all r. Sing s => r) -> r

-}

mkDoor :: Sing s -> Door a -> Door s
mkDoor _ door = UnsafeMkDoor (doorMaterial door)

someFn :: Proxy 'Opened -> Door 'Opened
someFn _ = UnsafeMkDoor "oak"

openDoor :: Door 'Opened
openDoor = someFn (Proxy :: Proxy 'Opened)

-- someFn_ :: forall a. DoorState -> Maybe (Door a)
-- someFn_ = \case
--   Opened -> Just openDoor
--   _ -> Nothing


unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor x door = if isOdd then Just $ mkDoor SClosed door else Nothing
  where
    isOdd =  x `mod` 2 /= 0

openAnyDoor :: forall s. SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Int -> Door s  -> Maybe (Door 'Opened)
    openAnyDoor_ doorState x door = case doorState of
      SOpened -> Just door
      SClosed -> Just $ mkDoor SOpened door
      SLocked -> mkDoor SOpened <$> unlockDoor x door
