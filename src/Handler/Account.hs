{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Handler.Account
  ( accountOperationH
  ) where

import           Control.Monad.IO.Class
import           Database.Persist.Sql
import           Servant

import           Models

accountOperationH pool = getAccountH pool :<|> putAccountH pool :<|> deleteAccountH pool

getAccountH :: ConnectionPool -> AccountId -> Handler (Maybe Account)
getAccountH pool =
  liftIO . getAccount
  where
    getAccount :: AccountId -> IO (Maybe Account)
    getAccount accountId = flip runSqlPersistMPool pool $ get accountId

putAccountH :: ConnectionPool -> AccountId -> Account -> Handler NoContent
putAccountH pool accountId body =
  liftIO (putAccount accountId body) >> return NoContent
  where
    putAccount :: AccountId -> Account -> IO ()
    putAccount accountId account = flip runSqlPersistMPool pool $ repsert accountId account

deleteAccountH :: ConnectionPool -> AccountId -> Handler NoContent
deleteAccountH pool accountId =
  liftIO (deleteAccount accountId) >> return NoContent
  where
    deleteAccount :: AccountId -> IO ()
    deleteAccount accountId = flip runSqlPersistMPool pool $ delete accountId

