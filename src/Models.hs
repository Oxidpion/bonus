{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Text

import           Database.Persist.Sql
import           Database.Persist.TH

import           Data.Time.Clock
import           Config

runDB :: (MonadIO m) => SqlPersistT IO b -> ReaderT App m b
runDB query = do
  pool <- asks appConnPool
  liftIO $ runSqlPool query pool

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account
  name Text
  deriving Eq Read Show
Transaction
  accountId AccountId
  date UTCTime
  amount Int
|]

instance FromJSON Account where
  parseJSON = withObject "Account" $ \ o ->
    Account <$> o .: "name"

instance ToJSON Account where
  toJSON (Account name) = object [ "name" .= name ]
