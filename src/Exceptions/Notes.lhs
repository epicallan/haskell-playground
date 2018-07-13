> module Exceptions.Notes where 

Here are a collection of notes from various sources on Exceptions handling in haskell

- All code must ideally be async-exception safe. 

- It is almost always wrong to wrap an ExceptT, EitherT, or ErrorT around an IO-based transformer stack

- Prefer to use ExceptT in pure functions

- Generally the solution to the ExceptT IO anti-pattern is to return an Either from more functions and 
 throw an exception for uncommon errors. 
 Note that returning Either from ExceptT IO means there are now 3 distinct sources of errors in just one function.

- masking async exceptions as an anti pattern

- Use the bracket pattern wherever possible.

- Use MonadThrow for composebility
consider code below

< foo <- lookup "foo" :: m a
< f foo :: Maybe a

if f returns Nothing we have no idea why thats the case.
An improvment could be 

< lookup :: Eq k => k -> [(k, v)] -> Either (KeyNotFound k) v
< f :: SomeVal -> Either F'sExceptionType F'sResult

issue with this is that the types don't unify. 

solution use MonadThrow

< lookup :: (MonadThrow m, Eq K) => k -> [(k, v)] -> m v
< f :: (MonadThrow m, Eq a) => SomeVal -> m a


- custom Exception string types is an anti-type

< foo = do
<    if x then return y else error "something bad happened"


side Notes


it's also almost always a bad idea to have such a concrete transformer stack used in a public-facing API. 
It's usually better to express a function in terms of typeclass requirements, using mtl typeclasses as necessary.

< foo :: Int -> IO String

This can always be generalized with a usage of liftIO to:

< foo :: MonadIO m => Int -> m String

