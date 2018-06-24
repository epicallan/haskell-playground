
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}
module ConduitEx where

import ClassyPrelude.Conduit
import Control.Concurrent (threadDelay)

source :: Monad m => ConduitM i Int m ()
source = do
    repeatC 5
    yieldMany [11..15]

-- sink :: Monad m => ConduitM Int o m (String, Int)
-- sink = do
--     x <- takeC 5 .| mapC show .| foldC
--     y <- sumC
--     return (x, y)

main :: IO ()
main =
    runConduit $ source  .| mapMC delayItem .| takeC 20 .| mapM_C print
    where
        delayItem x = liftIO (threadDelay 500000) $> x

-- TODO: have 2 async actions, one producing values, the other doing expensive computations
-- once the one with expensive computations completes, terminate the first

