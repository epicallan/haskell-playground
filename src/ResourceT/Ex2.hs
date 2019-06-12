{-# LANGUAGE BangPatterns #-}

module ResourceT.Ex2 where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import ResourceT.ResourceIO
import qualified System.IO as IO

data ListT m a
  = ConsT a (m (ListT m a))
  | NilT

appendListT :: Monad m
            => m (ListT m a)
            -> m (ListT m a)
            -> m (ListT m a)
appendListT left0 right =
    loop left0
  where
    loop mnext = do
      next <- mnext
      case next of
        ConsT x mnext' -> return $ ConsT x $ loop mnext'
        NilT           -> right

sourceHandle :: MonadIO m => IO.Handle -> m (ListT m B.ByteString)
sourceHandle h = liftIO $ do
  next <- B.hGetSome h 4096
  if B.null next
    then do
      IO.hClose h
      return NilT
    else return $ ConsT next (sourceHandle h)

sourceFile :: FilePath -> ResourceIO (ListT ResourceIO B.ByteString)
sourceFile fp = do
  h <- openBinaryFile fp IO.ReadMode
  sourceHandle h

sourceLength :: Monad m => m (ListT m B.ByteString) -> m Int
sourceLength =
    loop 0
  where
    loop !total mnext = do
      next <- mnext
      case next of
        ConsT bs mnext' -> loop (total + B.length bs) mnext'
        NilT            -> return total

main :: IO ()
main = do
  len <- runResourceIO $ sourceLength $ appendListT
    (sourceFile "/usr/share/dict/words")
    (sourceFile "/usr/share/dict/words")
  print len
