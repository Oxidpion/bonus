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

import Data.Aeson
import Data.Text

import Database.Persist.TH
import Data.Time.Clock

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
