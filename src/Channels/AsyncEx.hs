{-# LANGUAGE OverloadedStrings #-}
module Channels.AsyncEx where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

sayString :: Text -> IO ()
sayString = S8.putStrLn . encodeUtf8

talker :: Text -> IO ()
talker str = forever $ do
  sayString str
  threadDelay 500000

getResult :: IO Int
getResult = do
  sayString "Doing some big computation..."
  threadDelay 2000000
  sayString "Done!"
  return 42

main :: IO ()
main = do
  async1 <- async $ talker "async"
  withAsync (talker "withAsync") $ \_async2 -> do
    async3 <- async getResult

    res <- poll async3
    case res of
      Nothing        -> sayString "getResult still running"
      Just (Left e)  -> sayString $ "getResult failed: " <> pack (show e)
      Just (Right x) -> sayString $ "getResult finished: " <> pack (show x)

    res1 <- waitCatch async3
    case res1 of
      Left e  -> sayString $ "getResult failed: " <> pack (show e)
      Right x -> sayString $ "getResult finished: " <> pack (show x)

    res2 <- wait async3
    sayString $ "getResult finished: " <> pack (show (res2 :: Int))

  sayString "withAsync talker should be dead, but not async"
  threadDelay 2000000

  sayString "Now killing async talker"
  cancel async1

  threadDelay 2000000
  sayString "Goodbye!"
