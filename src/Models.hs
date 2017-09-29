{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}

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
  change Int
|]

instance FromJSON Account where
  parseJSON = withObject "Account" $ \ o ->
    Account <$> o .: "name"

instance ToJSON Account where
  toJSON (Account name) = object [ "name" .= name ]

instance ToJSON Transaction where
  toJSON t = object
    [ "date"    .= transactionDate t
    , "change"  .= transactionChange t
    ]

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    transactionDate       <- o .: "date"
    transactionChange     <- o .: "change"
    let transactionAccountId = AccountKey 0
    return Transaction {..}
