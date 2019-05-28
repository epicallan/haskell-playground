{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType       #-}
{-# LANGUAGE TypeOperators #-}
module Singleton.Ex4 where

import Data.Kind
import Data.Singletons.Prelude hiding (And, Or, sFoldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Foldr)
import Data.Singletons.TH hiding (sFoldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Foldr, sFold, Fold)
import Data.Singletons.Sigma
-- import Data.Void


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

$(singletons[d|
  mergeState :: DoorState -> DoorState -> DoorState
  mergeState Opened _      = Closed
  mergeState Closed Opened = Closed
  mergeState Closed Closed = Closed
  mergeState Closed Locked = Locked
  mergeState Locked _      = Locked
 |])

-- Alternatively, taking advantage of the derived Ord instance:
-- $(singletons [d|
--   mergeState :: DoorState -> DoorState -> DoorState
--   mergeState = max
--   |])

mergeDoor :: Door s -> Door t -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (MkSomeDoor s1 d1) (MkSomeDoor s2 d2) =
  MkSomeDoor (sMergeState s1 s2) $ mergeDoor d1 d2

-- representing a series of hallways
infixr 5 :>

data Hallway :: [DoorState] -> Type where
  HEnd :: Hallway '[]
  (:>) :: Door x -> Hallway xs -> Hallway (x ': xs)

{-
  ghci> let door1 = mkDoor SClosed "Oak"
  ghci> let door2 = mkDoor SOpened "Spruce"
  ghci> let door3 = mkDoor SLocked "Acacia"
  ghci> :t door1 :<# door2 :<# door3 :<# HEnd
  Hallway '[ 'Closed, 'Opened, 'Locked ]
-}


$(singletons[d|
  mergeStateList :: [DoorState] -> DoorState
  mergeStateList []        = Opened
  mergeStateList (x : xs ) = x `mergeState` (mergeStateList xs)
 |])

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd    =  mkDoor SOpened "End of Hallway"
collapseHallway (d :> ds) =  d `mergeDoor ` collapseHallway ds

{-
ghci> collapseHallway (door1 :<# door2 :<# door3 :<# HEnd)
UnsafeMkDoor "Oak and Spruce and Acacia and End of Hallway"
    :: Door 'Locked
-}

{-
data TyFun a b

type a ~> b = TyFun a b -> Type
s
infixr 0 ~>

data Id :: a ~> a
The “actual” kind of Id is Id :: TyFun a a -> Type;

Now, Id is not a function…it’s a dummy type constructor that represents a function a -> a.
A type constructor of kind a ~> a represents a defunctionalization symbol – a type constructor
that represents a function from a to a.

To interpret it, we need to write our global interpreter function

type family Apply (f :: a ~> b) (x :: a) :: b

-}

-- defunctionalization example
data NNot :: Bool ~> Bool

type instance Apply NNot 'True = 'False

type instance Apply NNot 'False = 'True

{-
type synonym

type f @@ a = Apply f a

infixl 9 @@

type instance Not @@ 'True = 'False

For a type family such as Not, singleton lib would generate sing instances and also
defunctionalization symbols according to a naming convention

data NotSym0 :: Bool ~> Bool
type instance Apply NotSym0 x = Not x

-- also generated for consistency
type NotSym1 x = Not x

-}

{-

$(singletons [d|
  and :: Bool -> (Bool -> Bool)
  and False _ = False
  and True  x = x
 |])

This will generate below types

type family And (x :: Bool) (y :: Bool) :: Bool where
    And 'False x = 'False
    And 'True  x = x

sAnd :: Sing x -> Sing y -> Sing (And x y)
sAnd SFalse x = SFalse
sAnd STrue  x = x

data AndSymo :: Bool ~> Bool ~> Bool
type instance Apply AndSym0 x = AndSymb1 x

data AndSym1 :: Bool -> Bool ~> Bool
type instance Apply (AndSym1 x) y = And x y

type AndSym2 x y = And x y

ghci> :kind! AndSym0 @@ 'False
AndSym1 'False

ghci> :kind! AndSym1 'False @@ 'True
'False              -- or FalseSym0, which is a synonym for 'False
ghci> :kind! AndSym1 'True  @@ 'True
'True
-}

{-
Symbols for type constructors

data TyCon1 :: (j -> k) -> (j ~> k)

-- alternatively
-- data TyCon1 (t :: j -> k) :: j ~> k

ghci> :kind! TyCon1 Maybe @@ Int
Maybe Int

ghci> :kind! TyCon1 'Right @@ 'False
'Right 'False

-}

{-

type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs

-}


{-
 $(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)

  mmergeState :: DoorState -> DoorState -> DoorState
  mergeState = max

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)

  -- MergeStateList type will be defined in terms of Foldr as
  -- type MergeStateList ss = Foldr MergeStateSym0 'Opened ss

  mmergeStateList :: [DoorState] -> DoorState
  mergeStateList = foldr mergeState Opened
  |])
-}



$(singletons [d|
  instance Semigroup DoorState where
      (<>) = mergeState
  instance Monoid DoorState where
      mempty  = Opened
      mappend = (<>)
  |])

$(singletons [d|
  fold :: Monoid b => [b] -> b
  fold []     = mempty
  fold (x:xs) = x <> fold xs
 |])

collapseHallway'
    :: Hallway ss
    -> Door (Fold ss) -- defining MergeStateList in terms of a Fold ss thanks to monoid instances
collapseHallway' HEnd      = UnsafeMkDoor "End of Hallway"
collapseHallway' (d :> ds) = d `mergeDoor` collapseHallway' ds


{-
 new some door with sigma as a dependent pair parameterized over Door State

data Sigma k :: (k ~> Type) -> Type where
    (:&:) :: Sing x -> (f @@ x) -> Sigma k f


-}

type NewSomeDoor = Sigma DoorState (TyCon1 Door)

mkSomeDoor :: DoorState -> String -> NewSomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
  dsSing :&: mkDoor dsSing mat


type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

collapseSomeHallway :: SomeHallway -> NewSomeDoor
collapseSomeHallway (ss :&: d) = sMergeStateList ss
                             :&: collapseHallway d

-- Q1

data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked

mergedIsKnockable
    :: Knockable s
    -> Knockable t
    -> Knockable (MergeState s t)
mergedIsKnockable = \case
    KnockClosed -> \case
      KnockClosed -> KnockClosed
      KnockLocked -> KnockLocked    -- GHC makes sure we use KnockLocked!
    KnockLocked -> \case
      KnockClosed -> KnockLocked
      KnockLocked -> KnockLocked

-- 2

type family AppendHallways (ss :: [DoorState]) (ts :: [DoorState]) :: [DoorState] where
  AppendHallways ss '[] = ss
  AppendHallways '[] ts = ts
  AppendHallways (x ': xs) (y ': ys)  = MergeState x y ': AppendHallways xs ys


appendHallways :: Hallway ss -> Hallway ts -> Hallway (AppendHallways ss ts)
appendHallways ss HEnd =  ss
appendHallways HEnd ts = ts
appendHallways (x :> xs) (y :> ys) = x `mergeDoor` y :> appendHallways xs ys

$(singletons [d|
  append :: [a] -> [a] -> [a]
  append []     ys = ys
  append (x:xs) ys = x : append xs ys
  |])

{-

type family Append (ss :: [DoorState]) (ts :: [DoorState]) :: [DoorState] where
  Append '[] ts = ts
  Append (x ': xs) ts = x ': Append xs ts
-}

appendHallways_ :: Hallway ss -> Hallway ts -> Hallway (Append ss ts)
appendHallways_ HEnd xs = xs
appendHallways_ (x :> xs) ys = x :> appendHallways_ xs ys

appendSomeHallways :: SomeHallway -> SomeHallway -> SomeHallway
appendSomeHallways (sh :&: hw) (sh2 :&: hw2)  =
  sAppend sh sh2 :&:  appendHallways_ hw hw2

mkSomeHallway :: [NewSomeDoor] -> SomeHallway
mkSomeHallway [] = SNil :&: HEnd
mkSomeHallway ((s :&: d): ds)  = case mkSomeHallway ds of
  ss :&: hw -> (s `SCons` ss) :&: (d :> hw)
