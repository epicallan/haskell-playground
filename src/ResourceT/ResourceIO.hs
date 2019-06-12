{-# LANGUAGE DeriveFunctor #-}
module ResourceT.ResourceIO where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified System.IO as IO

{-
In order to make this work, we'll implement a simplified version of ResourceT.
We'll keep a list of file handles that need to be closed.
But since we need to be able to update that list dynamically from within our continuation code,
this will be a mutable list (wrapped in an IORef).
Also, for simplicity, we'll make it ResourceIO instead of a proper monad transformer.
-}

newtype ResourceIO a = ResourceIO (IORef [IO.Handle] -> IO a)
  deriving Functor

instance Applicative ResourceIO where
  pure x = ResourceIO $ \_ -> return x
  (<*>) = ap

instance Monad ResourceIO where
  return = pure
  ResourceIO f >>= g = ResourceIO $ \ref -> do
    x <- f ref
    let ResourceIO g' = g x
    g' ref
instance MonadIO ResourceIO where
  liftIO m = ResourceIO $ \ _ -> m

runResourceIO :: ResourceIO a -> IO a
runResourceIO (ResourceIO inner) = bracket
  (newIORef [])
  cleanup
  inner
  where
    cleanup ref = do
      handles <- readIORef ref
      mapM_ IO.hClose handles

openBinaryFile :: FilePath -> IO.IOMode -> ResourceIO IO.Handle
openBinaryFile fp mode = ResourceIO $ \ref -> mask $ \restore -> do
  h <- restore $ IO.openBinaryFile fp mode
  atomicModifyIORef' ref $ \hs -> (h:hs, ())
  return h
