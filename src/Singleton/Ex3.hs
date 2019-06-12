{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType       #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Singleton.Ex3 where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Void

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

data Door :: DoorState -> Type where
  UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkDoor :: Sing s -> String -> Door s
mkDoor _  = UnsafeMkDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
  SOpened -> Disproved $ \case {}
  SClosed -> Proved KnockClosed
  SLocked -> Proved KnockLocked

knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
  Proved k    -> knock k d
  Disproved _ -> putStrLn "can't knock"

data Lockable :: DoorState  -> Type where
  LockableClosed :: Lockable 'Closed

isLockable :: Sing s -> Decision (Lockable s)
isLockable = \case
  SClosed  -> Proved LockableClosed
  SOpened  -> Disproved $ \case {}
  SLocked  -> Disproved $ \case {}

lock :: Lockable s -> Door s -> Door 'Locked
lock _ d = UnsafeMkDoor (doorMaterial d)

lockSomeDoor :: SomeDoor -> Maybe SomeDoor
lockSomeDoor (MkSomeDoor s d) = case isLockable s of
  Proved k    -> Just . fromDoor SLocked $ lock k d
  Disproved _ -> Nothing

lockDoor ::  Door 'Closed -> SomeDoor
lockDoor = fromDoor SLocked . UnsafeMkDoor . doorMaterial


-- using type level function for proves

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Eq)
 |])

type family StatePass (s :: DoorState) :: Pass where
  StatePass 'Opened = 'Allow
  StatePass 'Closed = 'Obstruct
  StatePass 'Locked = 'Obstruct


knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

sStatePass :: Sing s -> Sing (StatePass s)
sStatePass = \case
  SOpened -> SAllow
  SClosed -> SObstruct
  SLocked -> SObstruct

knockSomeDoorP :: SomeDoor -> IO ()
knockSomeDoorP (MkSomeDoor s d) = case sStatePass s of
  SAllow    -> putStrLn "No knocking allowed"
  SObstruct -> knockP d

{-
sStatePass can be defined by template haskell as below

$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])
-}

refuteRefuteKnockable :: forall s . SingI s => Refuted (Refuted (Knockable s)) -> Knockable s
refuteRefuteKnockable rrk = case isKnockable sing of
  Proved k     -> k
  Disproved rk -> absurd (rrk rk)

knockedRefute :: forall s. SingI s => Knockable s -> Refuted (s :~: 'Opened)
knockedRefute = \case
  KnockClosed -> \case {}
  KnockLocked -> \case{}

refuteKnocked :: forall s. SingI s => Refuted (s :~: 'Opened) -> Knockable s
refuteKnocked v = case sing @s of
  SOpened -> absurd $ v (Refl @ 'Opened)
  SClosed -> KnockClosed
  SLocked -> KnockLocked


knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "knock knock on " <> doorMaterial d <> " door"

{-
class SDecide k where
    (%~) :: Sing (a :: k)
         -> Sing (b :: k)
         -> Decision (a :~: b)

(STrue %~) :: Sing b -> Decision ('True :~: b)
which is a decision function to check if b is equal to 'True.
You can sort of imagine SDecide as a type-level Eq typeclass, but for “type equality”.

-}

knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =
    case sStatePass s %~ SObstruct of -- sing equality enabled SDecide
      Proved r    -> knockRefl r d
      Disproved _ -> putStrLn "No knocking allowed!"

$(singletons [d|
  invertPass :: Pass -> Pass
  invertPass Obstruct = Allow
  invertPass Allow    = Obstruct
 |])

{-
 this defines

 sInvertPass :: Sing s -> Sing (InvertPass s)
 sInvertPass = \case
   SAllow -> SObstruct
   SObstruct -> SAllow

 type family InvertPass (s :: Pass) :: Pass where
   InvertPass 'Allow  = 'Obstruct
   InvertPass 'Obstruct = 'Allow
-}

knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d =  putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorInv :: SomeDoor -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =
  case sInvertPass (sStatePass s) of
    SAllow    -> knockInv d
    SObstruct ->  putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])

instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed

{-
class PEq a where
    type (x :: a) == (y :: a) :: Bool       -- ^ associated type / type family
    type (x :: a) /= (y :: a) :: Bool

class SEq a where
    (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
    (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)
-}

instance PCycle DoorState where
  type Next 'Opened = 'Closed
  type Next 'Closed = 'Locked
  type Next 'Locked = 'Opened

  type Prev 'Opened = 'Locked
  type Prev 'Closed = 'Opened
  type Prev 'Locked = 'Closed

instance SCycle DoorState where
  sNext :: Sing (x :: DoorState) -> Sing (Next x)
  sNext = \case
    SOpened -> SClosed
    SClosed -> SLocked
    SLocked -> SOpened

  sPrev :: Sing (x :: DoorState) -> Sing (Prev x)
  sPrev = \case
    SOpened -> SLocked
    SClosed -> SOpened
    SLocked -> SClosed
