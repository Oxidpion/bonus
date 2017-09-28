{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Monad.Logger (runStderrLoggingT)
import           Database.Persist.Sqlite
import           Database.Persist.Sql
import           Network.Wai.Handler.Warp as Warp (HostPreference)

data App = App
  { appSettings :: AppSettings
  , appConnPool :: ConnectionPool
--  , appLogger   :: Logger
  }

data AppSettings = AppSettings
  { appHost :: HostPreference
  , appPort :: Int
  , appDatabaseConf :: SqliteConf
  }

mkSettings :: HostPreference -> Int -> SqliteConf -> IO AppSettings
mkSettings host port databaseConf = return AppSettings
  { appHost = host
  , appPort = port
  , appDatabaseConf = databaseConf
  }

mkApp:: IO App
mkApp = do
  settings <- mkSettings "*4" 3000 SqliteConf {sqlDatabase = "sqlite.db", sqlPoolSize = 5}
  pool <- runStderrLoggingT $
    createSqlitePool
      (sqlDatabase $ appDatabaseConf settings)
      (sqlPoolSize $ appDatabaseConf settings)

  return App {appSettings = settings, appConnPool = pool}