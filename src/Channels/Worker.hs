module Channels.Worker where

import           Control.Concurrent
import           Control.Monad      (forever)

worker :: Chan String -> (Chan String -> IO ()) -> IO ThreadId
worker chan foo = forkIO $ forever $ foo chan

worker2 :: IO () -> IO ThreadId
worker2 action = forkIO $ forever action

gossipGirl :: Chan String -> IO ()
gossipGirl chan = do
    gossip <- readChan chan
    putStrLn gossip

main :: IO ()
main = do
    putStrLn "Lets do some gossips"
    gossipChan <- newChan -- lets make new chan
    gossipChan2 <- newChan -- lets make new chan
    worker gossipChan gossipGirl -- spawn gossipGirl

    writeChan gossipChan "Garbage is garbage!"
    writeChan gossipChan "Garbage is garbage for reals!"

    worker2 (gossipGirl gossipChan2) -- woker2 2 girl!
    writeChan gossipChan2 "Umkay"
    writeChan gossipChan2 "Yez!"

    getLine
    putStrLn "Thank You Sir for Info"
