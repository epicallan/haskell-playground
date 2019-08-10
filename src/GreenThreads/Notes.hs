-- | based off https://www.fpcomplete.com/blog/2017/01/green-threads-are-like-garbage-collection

module GreenThreads.Notes where

-- import qualified Data.ByteString.Char8 as B8
-- import Data.Conduit
-- import Data.Conduit.Network

-- main = do
--     putStrLn "OK, I'm running!"

--     -- This automatically binds a new listening socket and forks a new
--     -- green thread for each incoming connection.
--     runTCPServer settings app
--   where
--     -- Listen on all interfaces on port 4500
--     settings = serverSettings 4500 "*"

-- -- Create a simple pipeline connecting the input from the network
-- -- (appSource) to our echo program to the output to the network
-- -- (appSink).
-- app appData = runConduit (appSource appData .| echo .| appSink appData)

-- -- awaitForever keeps running the inner function as long as new data
-- -- is available from the client
-- echo = awaitForever (\bytes -> do
--     -- Echo back the raw message we received
--     yield bytes
--     -- And now send back the Fibonacci value at the length of the
--     -- input. We need to use B8.pack to convert our String into a
--     -- binary format we can send over the network.
--     yield (B8.pack (show (fib (B8.length bytes)) ++ "\n")))

-- -- Written badly for high CPU usage!
-- fib 0 = 1
-- fib 1 = 1
-- fib n = fib (n - 1) + fib (n - 2)
