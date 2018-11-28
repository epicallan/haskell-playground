module ErrorHandling where

import           Control.Applicative ()
import           Control.Concurrent  (threadDelay)
import           Control.Exception   (SomeException)
import qualified Control.Exception   as E
import           Control.Monad
import           Data.IORef
import           System.IO.Unsafe    (unsafePerformIO)
import           System.Timeout      (timeout)

{-# NOINLINE numHandles #-}
numHandles :: IORef Int
numHandles = unsafePerformIO $ newIORef 0

{-# NOINLINE dataWritten #-}
dataWritten :: IORef [String]
dataWritten = unsafePerformIO $ newIORef []

test :: IO () -> IO ()
test action =
  action `E.catch` \e -> do
    putStrLn $ "exception: " ++ show (e :: SomeException)
    readIORef numHandles >>= putStrLn . ("Number of open handles: " ++) . show
    readIORef dataWritten >>= putStrLn . ("Data writtern to file: " ++) . show

newtype Handle = Handle (IORef (Maybe String))

openFile :: FilePath -> IO Handle
openFile _ = do
  modifyIORef' numHandles succ
  Handle <$> newIORef Nothing

hClose :: Handle -> IO ()
hClose h = hFlushFailing h
  `E.finally` modifyIORef numHandles pred

hFlushFailing :: Handle -> IO ()
hFlushFailing _ = error "hFlush failed"

hFlush :: Handle -> IO ()
hFlush (Handle ref) = do
  val <- readIORef ref
  case val of
    Just str -> modifyIORef dataWritten (str :)
    _        -> return ()
  writeIORef ref Nothing

hPutStr :: Handle -> String -> IO ()
hPutStr h@(Handle ref) str = do
  hFlush h
  writeIORef ref (Just str)

bracket :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket allocate release use =
  E.mask $ \restore -> do
    resource <- allocate
    restore (use resource)
      `E.finally` release resource

example :: IO ()
example = void $ timeout (1 * 1000 * 1000) $
  bracket (openFile "path") hClose $ \h -> do
    hPutStr h "Hello"
    hPutStr h "World"
    threadDelay (2 * 1000 * 1000)

main :: IO ()
main = test example

-- newtype MyException = MyException Text deriving Show

-- instance Exception MyException

-- newtype Env = Env { name :: Text } deriving (Functor, Show)

-- type App (m :: Monad) = ExceptT MyException m a

-- main' :: (MonadIO m, MonadReader Env m) => App m Text
-- main' = runExceptT $ do
--   _   <- test1
--   val <- test2
--   pure $ Right $ map toUpper val


-- test1 :: Monad m => App m ()
-- test1 = throwError $ MyException "some bad error"

-- test2 :: Monad m => App m Text
-- test2 = pure $ Right "success"
