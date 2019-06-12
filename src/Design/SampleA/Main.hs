{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Design.SampleA.Main where

import Universum

import Control.Lens hiding (view)

newtype Config = Config {_companyName :: Text}


data AppState
    = AppState
    { _asConfig :: Config -- could be an all blown out slice of app state
    , _env      :: Text
    }

makeLenses ''AppState
makeClassy ''Config

-- from makeClassy we get
-- class HasConfig a where
--     config :: Lens' a Config
--     companyName :: Lens' a Text

instance HasConfig AppState where
  config = asConfig

class (Monad m) => MonadStorage m where
    updateUser :: Text -> m ()
    getUsers :: m [Text]

-- we can also have an instance of MonadStorage where we don't use a MonadIO for testing purposes
instance (MonadIO m, HasConfig env) => MonadStorage (ReaderT env m) where
    updateUser username = do
        name <- view (config . companyName)
        liftIO $ putTextLn $ username <> name

    getUsers = do
        name <- view (config . companyName)
        liftIO $ putTextLn "logging IO action or DB action"
        pure [name, "users got from DB"]


getCompanyName :: (MonadReader r m, HasConfig r) => m Text
getCompanyName = view (config . companyName)

heavyReport :: (MonadReader AppState m) => m Text
heavyReport = do
  cn <- getCompanyName
  return (cn <> " is the best company!")

lightReport :: (MonadReader r m, HasConfig r) => m Text
lightReport = do
    cn <- getCompanyName
    return (cn <> " is the best company!")

getUsersReport :: (MonadStorage m, MonadReader r m, HasConfig r) => m (Text, [Text])
getUsersReport  = do
    report <- getCompanyName
    users  <- getUsers
    pure (report, users)

 -- we can then run all our readers in one place in main

runReport :: Config -> Text
runReport = runReader lightReport

runUsers :: Config -> IO (Text, [Text])
runUsers = runReaderT getUsersReport
