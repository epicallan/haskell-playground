{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Singleton.Notes where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH

{-|
source https://blog.jle.im/entry/introduction-to-singletons-1.html
-}

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

doorStatus :: Sing s -> Door s -> DoorState
doorStatus SOpened _ = Opened
doorStatus SClosed _ = Closed
doorStatus SLocked _ = Locked


lockAnyDoor :: Sing s -> Door s -> Door 'Locked
lockAnyDoor sings door = case sings of
    SOpened -> lockDoor . closeDoor $ door
    SClosed -> lockDoor door
    SLocked -> door

-- The singleton template haskell function creates a class SingI and GADT SignDs

-- a recreation of SingDs named SingDS to avoid name clashing
data SingDS :: (DoorState -> Type) where
  SDOpened :: SingDS 'Opened
  SDClosed :: SingDS 'Closed
  SDLocked :: SingDS 'Locked

-- a recreation of SingI as SingID to avoid name clashing
class SingID s where
  singd :: SingDS s

instance SingID 'Opened where
  singd = SDOpened

instance SingID 'Closed where
  singd = SDClosed

instance SingID 'Locked where
  singd = SDLocked

withSingDS :: SingDS s -> (SingID s => r) -> r
withSingDS sng x = case sng of
  SDOpened -> x
  SDLocked -> x
  SDClosed -> x


{--
  Essentially, our singletons give us runtime values that can be used as witnesses for types and type variables.
  These values exist at runtime, so they “bypass” type erasure.
  Types themselves are directly erased, but we can hold on to them using these runtime tokens when we need them.
--}

doorStatus__ :: forall s. SingID s => Door s -> DoorState
doorStatus__ = status singd
  where
    status :: SingDS s -> Door s -> DoorState
    status x _  = case x of
      SDOpened -> Opened
      SDLocked -> Locked
      SDClosed -> Closed

doorStatus_ :: SingI s => Door s -> DoorState
doorStatus_ = doorStatus sing

lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor
-- mkDoor SOpened "oak" --> Note that Sing s is only used to look in type value in Door s
-- Door 'Opened

{--
 Due to Singletons we have the following from DoorState

 - the type DoorState with value Constructors Opened, Closed and Locked
 - the kind DoorState whose type Constructors are 'Opened, 'Closed and 'Locked
 - Singletons for the 'Opened, 'Closed and 'Locked
 - The SingI instance for 'Opened, 'Closed and 'Locked
--}

-- singletons also generate fromSing which takes us from singletons to term-level values (reflection)
-- fromSing :: Sing (s :: DoorState) -> DoorState

-- exercise
-- function to unlock door if user enters an odd number

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n door = if isOdd
  then Just $ mkDoor SClosed (doorMaterial door)
  else Nothing
    where
      isOdd = n `mod` 2 /= 0

-- a function that can open any door taking a password in implicit Sing style

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n  = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SClosed -> Just . openDoor
      SOpened -> Just
      SLocked -> fmap openDoor . unlockDoor n

-- MkSomeDoor is a constructor for an existential data type,
-- meaning that the data type hides a type variable s.

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
  SOpened -> Just . fromDoor_ $ closeDoor d
  SClosed -> Nothing
  SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> Maybe SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = Just . fromDoor_ $ lockAnyDoor s d

{-
One main reason (besides allowing code-reuse like we did earlier) is that using the singleton
lets us directly recover the type. Essentially, a Sing s not only contains whether it is Opened/Closed/Locked
(like a DoorState would), but also it contains it in a way that GHC can use to bring it all back to the type level.

So, for our original Door s functions, we need to know s at runtime – storing the Sing s gives GHC exactly that. Once you get the Sing s back, you can now use it in all of our type-safe functions from Part 1, and you’re back in type-safe land

In the language of dependently typed programming, we call SomeDoor a dependent sum,
because you can imagine it basically as a sum type:

data SomeDoor = SDOpened (Door 'Opened)
              | SDClosed (Door 'Closed)
              | SDLocked (Door 'Locked)


You might also see SomeDoor called a dependent pair –
it’s a “tuple” where the type of the second item (our Door s)is determined
by the value of the first item (our Sing s).
-}


-- we can finally make a door with the status unknown until runtime

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
  Opened -> fromDoor_ . mkDoor SOpened
  Closed -> fromDoor_ . mkDoor SClosed
  Locked -> fromDoor_ . mkDoor SLocked

-- in Continuos Passing Style -style existential

withDoor
  :: DoorState
  -> String
  -> (forall s. Sing s -> Door s -> r)
  -> r
withDoor s m f = case s of
  Opened -> f SOpened (mkDoor SOpened m)
  Closed -> f SClosed (mkDoor SClosed m)
  Locked -> f SLocked (mkDoor SLocked m)


-- ghci > withDoor Opened "Oak" \s _ -> case s of
--    SOpened -> "Opened door"
--     _      -> "Not open"

{-
The general pattern we are exploring here is called reification.
The singletons library automatically generates functions to directly reify DoorState values

toSing       :: DoorState -> SomeSing DoorState
withSomeSing :: DoorState -> (forall s. Sing s        -> r) -> r
withSomeSing :: DoorState -> (forall s. SDoorState s  -> r) -> r
                                     -- ^ using the convenience type synonym

remember singleton lib gives us wrapper from Sing values to Corresponding runtime values

-- from singletons (not the actual definition, just psuedo-code to demonstrate
-- what the constructors look like)

data SomeSing DoorState :: Type where
    SomeSing :: Sing s -> SomeSing DoorState

instance values

SomeSing SOpened :: SomeSing DoorState
SomeSing SClosed :: SomeSing DoorState
SomeSing SLocked :: SomeSing DoorState

-}

mkSomeDoor_ :: DoorState -> String -> SomeDoor
mkSomeDoor_ ds = case toSing ds of
    SomeSing s -> fromDoor s . mkDoor s

withDoor_
  :: DoorState
  -> String
  -> (forall s. Sing s -> Door s -> r)
  -> r
withDoor_ ds m f = withSomeSing ds $ \s -> f s (mkDoor s m)


-- exercise
-- Let’s revisit our original redundant SomeDoor, compared to our final SomeDoor

data OldSomeDoor :: Type where
  OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

{-
 data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor
-}

-- To help convince yourself that the two are equal, write functions converting between the two

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor s d) = OldMkSomeDoor (fromSing s) (doorMaterial d)

{-
Previously, we had an unlockDoor function that took an Int (the “password”) with a Door 'Locked and returned a Maybe (Door 'Closed). It returns a Door 'Closed (unlocked door) in Just if an odd number was given, and Nothing otherwise (a failed unlock)

Use this to implement a that would return a SomeDoor. Re-use the “password” logic from the original unlockDoor. If the door is successfully unlocked (with a Just), return the unlocked door in a SomeDoor. Otherwise, return the original locked door (in a SomeDoor).
-}

-- unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n (UnsafeMkDoor m)
  | n `mod` 2 == 1 = mkSomeDoor_ Closed m
  | otherwise      = mkSomeDoor_ Locked m


openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n sd@(MkSomeDoor s d) = withSingI s $
    case openAnyDoor n d of
      Nothing -> sd
      Just d' -> fromDoor_ d'

-- write the SingKind instance for the promoted kind of a custom list type

data List a = Nil | Cons a (List a)

data instance Sing (x :: List k) where
  SNil :: Sing 'Nil
  SCons :: Sing x -> Sing xs -> Sing ('Cons x xs)

instance SingKind k => SingKind (List k) where
  type Demote (List k) = List (Demote k)

  fromSing :: Sing (xs :: List k) -> List (Demote k)
  fromSing SNil         = Nil
  fromSing (SCons x xs) = Cons (fromSing x) (fromSing xs)

  toSing :: List (Demote k) -> SomeSing (List k)
  toSing = \case
     Nil       -> SomeSing SNil
     Cons x xs -> withSomeSing x  $ \sx ->
                   withSomeSing xs $ \sxs ->
                      SomeSing (SCons sx sxs)

-- Dependently typed Proofs
-- given the function below

knock_ :: Door s -> IO ()
knock_ d = putStrLn $ "knock on " ++ doorMaterial d ++ " doo!"

-- this function can't be legal given we can't knock on closed doors
--  Hence we need to make it more restrictive

{-
Proofs are witnesses to some type level predicate or proposition
A value level predicate in haskell is a -> Bool

A type level predicate is a type constructor of kind k -> Type

We can define a predicate

Knockable :: DoorState -> Type

as a GADT that only has values if given 'Closed and 'Locked, but not 'Opened
-}

data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

-- We now make a version of knock that requires a proof that s is Knockable

knock :: Knockable s -> Door s  -> IO ()
knock _ d = putStrLn $ "knock knock on " ++ doorMaterial d ++ " door!"

-- ghci> knock KnockClosed (mkDoor SClosed "Birch")

{-
 This can be made more seamless by auto generating proofs at compile-time with a general class
 like Auto
-}

class Proved p a where
  auto :: p a

instance Proved Knockable 'Closed where
  auto = KnockClosed

instance Proved Knockable 'Locked where
  auto = KnockLocked

{-
ghci> knock auto (mkDoor SClosed "Acacia")
Knock knock on Acacia door!

ghci> knock auto (mkDoor SOpened "Jungle")
COMPILER ERROR!! COMPILER ERROR!!
-}

{-
 All this works only if you know what s is at compile-time. If you are retrieving s at runtime you need
 to take advantage of the decidability predicate
-}

-- decidePred :: Sing x -> Decision (P x)

data DDecision a =
    DProved a               -- ^ a value of a exists
  | DDisproved (DRefuted a) -- ^ a value of a cannot exist

-- | The data type with no values
data DVoid

-- | 'a' cannot exist.  Commonly also called `Not`
type DRefuted a = a -> DVoid

{-
Decision a is like a Maybe a, except instead of Nothing, we include a proof that the predicate is not true.

The a -> Void idiom (often called Not a, or Refuted a) is type we use in Haskell
and other languages to represent the fact that it is impossible to construct a value of type a.
-}

isKnockable :: Sing s -> DDecision (Knockable s)
isKnockable = \case
  SLocked -> DProved KnockLocked
  SClosed -> DProved KnockClosed
  SOpened -> DDisproved $ \case {}

disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of {}             -- empty pattern match

knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
  DProved k    -> knock k d
  DDisproved _ -> putStrLn "No knocking allowed"

{-
Singleton library exports a propositional equality

data (:~:) :: k -> k -> Type where
    Refl :: a :~: a

so 'Blah :~: a is  proof that a is equal to 'Blah

It also offers the kindclass SDecide which provides decision function for the a :~: predicate

class SDecide k where
    (%~) :: Sing (a :: k)
         -> Sing (b :: k)
         -> Decision (a :~: b)

For example, Bool is an instance of SDecide, so we have a function:

(STrue %~) :: Sing b -> Decision ('True :~: b)

which is a decision function to check if b is equal to 'True

SDecide is a typeleve Eq typeclass
-}

-- A different method useful for restricting function calling

$(singletons [d|
  data Pass = Obstruct | Allow
  |])

type family StatePass (s :: DoorState) :: Pass where
  StatePass 'Opened = 'Allow
  StatePass 'Closed = 'Obstruct
  StatePass 'Locked = 'Obstruct

knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock on " ++ doorMaterial d ++ " door!"

{-
a ~ b is a constraint for type equality. This constraint means that calling knock requires
that StatePass s is equal to (or unifies with) 'Allow.
-}

sStatePass :: Sing s -> Sing (StatePass s)
sStatePass = \case
    SOpened -> SAllow
    SClosed -> SObstruct
    SLocked -> SObstruct

knockSomeDoorP
    :: SomeDoor     -- ^ status not known until you pattern match at runtime
    -> IO ()
knockSomeDoorP (MkSomeDoor s d) = case sStatePass s of
    SObstruct -> knockP d                        -- ^ `StatePass s ~ 'Obstruct`
    SAllow    -> putStrLn "No knocking allowed!" -- ^ `StatePass s ~ 'Allow`

{-
 With singleton lib we can define type family StatePass with associated singleton functions as below

$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])

This will define
 - The type family StatePass (s :: DoorState) :: Pass, like we defined above
 - The singleton function sStatePass, with the type Sing s -> Sing (StatePass s), like we defined above.

-}
