-- | http://hackage.haskell.org/package/free-2.0.3/docs/src/Control-Monad-Free.html
{-# LANGUAGE DeriveFunctor #-}
module FreeMonads.Notes2 where

import Control.Monad.Free
import qualified System.Exit as S

data TeletypeF x
 = PutStrLn String x
 | GetLine (String -> x)
 | ExitSuccess
 deriving (Functor)


type Teletype = Free TeletypeF

-- | TODO: exercise

-- trace the executions below
-- Free (GetLine p) <$> f
-- Free (GetLine p) >>= f 

putStrLn' :: String -> Teletype ()
putStrLn' str = liftF $ PutStrLn str ()

getLine' :: Teletype String
getLine' = liftF $ GetLine id

existSuccess' :: Teletype r
existSuccess' = liftF ExitSuccess

run :: Teletype r -> IO r
run (Pure r)                = return r
run (Free (PutStrLn str t)) = putStrLn str >> run t
run (Free (GetLine f))      = getLine >>= run . f
run (Free ExitSuccess)      = S.exitSuccess

echo :: Teletype ()
echo = do
  _str <- getLine'
  _ <- existSuccess'
  putStrLn' "Finished"

main :: IO ()
main = run echo
