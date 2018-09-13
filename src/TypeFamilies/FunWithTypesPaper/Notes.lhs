> {-# LANGUAGE TypeFamilies          #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> module TypeFamilies.FunWithTypesPaper.Notes where
> import Universum
> import Data.STRef
> import Control.Monad.ST

TODO: turn this into a blog post

The problem / abstract

_________________________

One of the main purpose of type families is to express functional dependencies at a type level

consider IORef definition over IO

< newIORef   :: a -> IO (IORef a)
< readIORef  :: IORef a -> IO a
< writeIORef :: IORef a -> a -> IO ()

and also consider STRef definition over ST monad

< newSTRef   :: a -> ST s (STRef s a)
< readSTRef  :: STRef s a -> ST s a
< writeSTRef :: STRef s a -> a -> ST s ()

These operations could be overloaded this way 

> class Mutation' m r where
>   newRef'   :: a -> m (r a)
>   readRef'  :: r a -> m a
>   writeRef' :: r a -> a -> m ()

< instance Mutation' IO IORef where
<   newRef' = newIORef
<   ...etc...

< instance Mutation' (ST s) (STRef s) where
<   newRef' = newSTRef
<   ...etc...

disadavantages of this approach is that the types of newRef are too polymorphic.
i.e one could declare this 

< instance Mutation IO (STRef s) where ...

Even when we want to declare that IO monad can only have one refrence type named `IORef`

As a result of this polymorphisim, its easy to write such programs where the type checker 
cannot select the type of r since it can be anything. (look at previous instance def)

> readAndPrint :: IO ()
> readAndPrint = do 
>   r <- newRef "x"
>   v <- readRef r 
>   print v 


Types are no longer light weight when they have to be explicitly specified.
A solution to this is functional dependencies

< class Mutation m r | m -> r where  {}

"m -> r" state's m is related to only one r 

The solution: Associated types 
______________________________

If m is always associated to only one type with in class Mutation. Then it can be said 
class Mutation only requires one type parameter which is associated with another, which is 
functionally dependent on it. 

> class Mutation m where 
>   type Ref m :: * -> *
>   newRef   :: a -> m (Ref m a)
>   readRef  :: Ref m a -> m a
>   writeRef :: Ref m a -> a -> m ()

> instance Mutation IO where 
>   type Ref IO = IORef 
>   newRef   = newIORef
>   readRef  = readIORef 
>   writeRef = writeIORef


> instance Mutation (ST s) where
>   type Ref (ST s) = STRef s 
>   newRef   = newSTRef 
>   readRef  = readSTRef 
>   writeRef = writeSTRef


It can be said that Ref is a type family or an associated type of class Mutation. 
`Ref m a` means applying type function to m.

Explicit Type coercion with associated types
___________

Say you want to have a polymorphic version of add that can work with an integer 
and a double and always return a Double

> class Add a b where 
>   type SumTy a b 
>   add :: a -> b -> SumTy a b 

> instance Add Integer Double where 
>   type SumTy Integer Double = Double
>   add x y = fromIntegral x + y 

> instance Add Double Integer where 
>   type SumTy Double Integer = Double 
>   add x y = x + fromIntegral y 

> instance (Num a) => Add a a where 
>   type SumTy a a = a 
>   add x y = x + y 


In  other  words, SumTy is a two-argument type function that maps the
argument types of an addition to type of its result.  

