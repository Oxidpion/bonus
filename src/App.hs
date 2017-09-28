{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.Trans.Reader
import           Database.Persist.Sqlite
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Data.Text

import           Api
import           Models
import           Config
import           Handler.Account (accountOperationH)

warpSettings :: App -> Settings
warpSettings setting =
    Warp.setPort (appPort $ appSettings setting)
  $ Warp.setHost (appHost $ appSettings setting)
    Warp.defaultSettings

server :: App -> Server Api
server app =
  enter (convertType app) accountOperationH
  where
    convertType :: App -> ReaderT App Handler :~> Handler
    convertType = runReaderTNat

warpApplication :: App -> Application
warpApplication = serve api . server

run :: IO ()
run = do
  app <- mkApp
  runSqlPool (runMigration migrateAll) (appConnPool app)
  Warp.runSettings (warpSettings app) (warpApplication app)
