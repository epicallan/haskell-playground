module ImplicitParams.Class where

import Unsafe.Coerce


-- TODO: write blog post about implicit params

-- | Dirty hack to pass values implicitly
-- This takes advantage of the fact that Haskell's runtime
-- de-sugars type class constraints into compiler generated dictionaries
-- that are passed to functions with the respective constraints

class Implicit a where
  param :: a

newtype Param a r = Param ( Implicit a => r )

infixl 1 $~

($~) :: forall a r. (Implicit a => r) -> a -> r
f $~ x = f' x
  where
    f' :: a -> r
    f' = unsafeCoerce (Param @a f)
    

data Config = Config deriving Show

app :: Implicit Config => IO ()
app = run param
 where
   run :: Config -> IO ()
   run config = putStrLn $ "App is running with config: " <> show config

main :: IO ()
main = app $~ Config
