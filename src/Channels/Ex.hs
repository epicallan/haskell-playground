module Channels.Ex where

import Control.Concurrent
import Control.Monad (forever)


data AskForMeow = GibFood | Smile

meowMe :: Chan AskForMeow -> Chan String -> IO ()
meowMe chan chanBack = do
  niceTry <- readChan chan
  case niceTry of
    GibFood -> writeChan chanBack "Meow"
    Smile   -> writeChan chanBack "No"

cat :: IO () -> IO ThreadId
cat action = forkIO $ forever action

main :: IO ()
main = do
  putStrLn "Hey kitty kitty"

  foodInputChan <- newChan
  catOutputChan <- newChan

  _ <- cat $ meowMe foodInputChan catOutputChan -- forked and would have been blocking if not

  writeChan foodInputChan Smile
  response <- readChan catOutputChan
  putStrLn response

  writeChan foodInputChan GibFood
  response' <- readChan catOutputChan
  putStrLn response'

  _ <- getLine
  return ()
