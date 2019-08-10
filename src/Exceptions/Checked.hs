-- | source
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
module Exceptions.Checked where

-- | custom prelude
import Universum

{-
 This is an extended take and exploration on implementing Checked exemptions in haskell
 inspired by  https://www.well-typed.com/blog/2015/07/checked-exceptions/
-}

class (MonadThrow m, Exception e) => Throws e m where
  throwChecked :: e -> m a

  default throwChecked ::  e -> m a
  throwChecked = throwM

-- | @m@ is of kind monad, @ts@ is a list of Exception types
type family ThrowsMany (m :: Type -> Type) (ts :: [Type]) :: Constraint where
  ThrowsMany _ '[] = ()
  ThrowsMany m (e ': ts) = (Throws e m, ThrowsMany m  ts)

type Id  = String

data User = User
  { uName :: String
  , uAge  :: Int
  , uId   :: Id
  } deriving Show

class Monad m => HttpNetwork m a where
  getHttp :: Id -> m a
  updateHttp :: Id -> m a

data HTTPException = HTTPException
  deriving (Show)
  deriving anyclass (Exception)

data DBException = DBException
  deriving (Show)
  deriving anyclass (Exception)

-- | example of monadic effectfull  code that can throw multiple errors
simpleHttp
  :: forall m . (ThrowsMany m '[ HTTPException, DBException ], HttpNetwork m User)
  => Id -> m User
simpleHttp userId = do
  user <- getHttp userId
  case uId user of
     x | x == userId -> pure user
       | x == "Null" -> throwChecked DBException
       | otherwise   -> throwChecked HTTPException
