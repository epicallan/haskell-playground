> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-#LANGUAGE GADTs #-}

> module DataKinds.Intro where

> data Bool = True | False

data Bool definition introduces the following.

- a type constrcutor Bool of kind Type
- a data constrcutor True of type Bool
- a data constructor False of type Bool

With data kinds we get

- a Kind Bool
- a promoted data constrcutor True' of kind Bool
- a promoted data constrcutor False' of kind Bool

promoted data constrcutors are of the wrong kind to exist at run tume.
Hence without anyother extension they can be used as phantom parameters to
handle state transitions.

Example

> data LockState = UnLocked | Locked

> data Lock (s :: LockState) = Lock { getLocked :: Int }

> newLock :: IO (Lock 'UnLocked)
> newLock = pure $ Lock 10

> withLock :: Lock 'UnLocked -> (Lock 'Locked -> IO a) -> IO a
> withLock = undefined
