module ResourceT.Ex where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

main :: IO ()
main = runResourceT $ do
    (releaseKey, resource) <- allocate
        (do
            putStrLn "Enter some number"
            readLn)
        (\i -> putStrLn $ "Freeing scarce resource: " ++ show i)
    doSomethingDangerous resource
    liftIO $ putStrLn $ "Going to release resource immediately: " ++ show resource
    release releaseKey
    somethingElse

doSomethingDangerous :: Int -> ResourceT IO ()
doSomethingDangerous i =
    liftIO $ putStrLn $ "5 divided by " ++ show i ++ " is " ++ show (5 `div` i)

somethingElse :: ResourceT IO ()
somethingElse = liftIO $ putStrLn
    "This could take a long time, don't delay releasing the resource!"
