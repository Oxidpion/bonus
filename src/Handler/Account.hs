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
import           Control.Monad.Trans.Reader

import           Database.Persist.Sql
import           Servant

import           Models

accountOperationH =
       getAccountH
  :<|> putAccountH
  :<|> deleteAccountH

getAccountH :: AccountId -> ReaderT ConnectionPool Handler (Maybe Account)
getAccountH accountId = do
  pool <- ask
  liftIO $ flip runSqlPersistMPool pool $ get accountId

putAccountH :: AccountId -> Account -> ReaderT ConnectionPool Handler NoContent
putAccountH accountId account = do
  pool <- ask
  liftIO $ flip runSqlPersistMPool pool $ repsert accountId account
  return NoContent

deleteAccountH :: AccountId -> ReaderT ConnectionPool Handler NoContent
deleteAccountH accountId = do
  pool <- ask
  liftIO $ flip runSqlPersistMPool pool $ delete accountId
  return NoContent

