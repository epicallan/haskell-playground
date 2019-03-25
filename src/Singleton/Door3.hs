{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

-- Exercise

module Singleton.Door3 where

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude hiding (And, Or)
import           Data.Singletons.TH
import           Data.Void

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    MkSomeDoor dsSing (mkDoor dsSing mat)

data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

class Proved p a where
    auto :: p a

instance Proved Knockable 'Closed where
    auto = KnockClosed

instance Proved Knockable 'Locked where
    auto = KnockLocked

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
    SOpened -> Disproved $ \case {}    -- s ~ 'Opened
    SClosed -> Proved KnockClosed      -- s ~ 'Closed
    SLocked -> Proved KnockLocked      -- s ~ 'Locked

disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of {}             -- empty pattern match

knockSomeDoor
    :: SomeDoor     -- ^ status not known until you pattern match at runtime
    -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
    Proved k    -> knock k d
    Disproved _ -> putStrLn "No knocking allowed!"

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq)

  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])

knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorP
    :: SomeDoor     -- ^ status not known until you pattern match at runtime
    -> IO ()
knockSomeDoorP (MkSomeDoor s d) = case sStatePass s of
    SObstruct -> knockP d                        -- ^ `StatePass s ~ 'Obstruct`
    SAllow    -> putStrLn "No knocking allowed!" -- ^ `StatePass s ~ 'Allow`

{-
We talk about predicates as type constructors with type k -> Type. This fits a lot of things we’ve seen before (all instances of Functor, for example), but some predicates are more interesting than others.

What is the interpretation of SDoorState as a predicate? (remember, SDoorState s is the type synonym for Sing (s :: DoorState)) What “traditional” (that is, a -> Bool) predicate does it correspond to?

What is the type of its decision function? Can you implement it?
-}

decideDoorState :: Sing s -> Decision (SDoorState s)
decideDoorState = Proved

{-
Now let’s practice working with predicates, singletons, and negation via Refuted together.

You may have heard of the principle of “double negation”, where not (not p) implies p. So, we should be able to say that Refuted (Refuted (Knockable s)) implies Knockable s.8 If something is not “not knockable”, then it must be knockable, right?

Try writing refuteRefuteKnockable to verify this principle — at least for the Knockable predicate.
-}

refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))
    -> Knockable s
refuteRefuteKnockable rrK =
    case isKnockable sing of   -- sing @_ @s for singletons-2.4.1 and earlier
      Proved    k  -> k
      Disproved rK -> absurd (rrK rK) -- this line is confusing

knockedRefute
    :: forall s. SingI s
    => Knockable s
    -> Refuted (s :~: 'Opened)
knockedRefute = \case
  KnockClosed -> \case {} -- no constructors of type ('Opened' :~: 'Closed)
  KnockLocked -> \case {} -- no constructors of type ('Opened :~: 'Locked)

refuteKnocked
    :: forall s. SingI s
    => Refuted (s :~: 'Opened)
    -> Knockable s
refuteKnocked v = case sing @s of   -- sing @_ @s for singletons-2.4.1 and earlier
    SOpened -> absurd $ v (Refl @'Opened)
        -- in this branch, we have `s ~ 'Opened`, so we can use `v` with
        -- `'Opened :~: 'Opened`.
    SClosed -> KnockClosed
    SLocked -> KnockLocked