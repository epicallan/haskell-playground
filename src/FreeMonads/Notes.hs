{-# LANGUAGE InstanceSigs #-}
module FreeMonads.Notes where

-- | http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
data Toy b next =
    Output b next -- prints
  | Bell next -- rings computer bell
  | Done -- ends execution
  deriving Show

-- output "A"
outputA :: Toy Char (Toy b next)
outputA =  Output 'A' Done

-- | but unfortunately this doesn't work because every time
-- I want to add a command, it changes the type:
bell :: Toy a (Toy Char (Toy b next))
bell = Bell outputA

-- | Fortunately, we can cheat and use the following data type to wrap as many
-- Toys as we want into the same data type

newtype Cheat f = Cheat (f (Cheat f))

-- | Fortunately cheat already exist as Fix in haskell

newtype Fix f = Fix (f (Fix f))

fixedOutput :: Fix (Toy Char)
fixedOutput = Fix (Output 'A' (Fix Done))

fixedBell :: Fix (Toy Char)
fixedBell = Fix (Bell fixedOutput)

-- | This approach only works if we can use Done constructor to terminate
-- every chain of functors.

-- | a quick and dirty fix  where we Throw an exception and let whoever calls
-- our subroutine catch it and resume

data FixE f e = FixE (f (FixE f e)) | Throw e

catch
  :: Functor f
  => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (FixE x) f  = FixE ( fmap (`catch` f)  x)
catch (Throw e) f = f e


instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell next)     = Bell (f next)
  fmap _f Done           = Done

instance Functor (Toy b) => Applicative (Toy b) where
  pure :: a -> Toy b a
  pure _ = Done

  (<*>) :: Toy b ( a -> c) -> Toy b a -> Toy b c
  _x <*> _y = error "do me"

data IncompleteException = IncompleteException

-- output A
-- throw IncompleteException
subroutine :: FixE (Toy Char) IncompleteException
subroutine = FixE (Output 'A' (Throw IncompleteException))

program :: FixE (Toy Char) e
program = subroutine `catch` (\ _ -> FixE (Bell (FixE Done)))

-- | FixE as a Free Monad

data Free f r = Free (f (Free f r)) | Pure r

instance Functor f => Functor (Free f) where
  fmap :: (a -> b) -> Free f a ->  Free f b
  fmap f (Pure a)  = Pure $ f a
  fmap f (Free fa) = Free (fmap f <$> fa)


instance Functor f => Applicative (Free f) where
  pure :: a -> Free f a
  pure = Pure

  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  Pure fa <*> Pure a = Pure $ fa a
  Pure fa <*> Free mb = Free $ fmap fa <$> mb
  Free fa <*> b = Free $ (<*> b) <$> fa

instance (Functor f, Applicative f) => Monad (Free f) where
  return = Pure
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Free x >>= f = Free (fmap (>>= f) x)
  Pure r >>= f = f r

liftT :: Functor f => f r -> Free f r
liftT command = Free (fmap Pure command)

freeSubroutine :: Free (Toy Char) ()
freeSubroutine = liftT $ Output 'A' ()

freeBell :: Free (Toy Char) ()
freeBell = liftT (Bell ())

freeDone :: Free (Toy Char) ()
freeDone = liftT Done

freeProgram :: Free (Toy Char) ()
freeProgram = do
  freeSubroutine
  freeBell
  freeDone

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"
