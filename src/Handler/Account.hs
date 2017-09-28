{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Handler.Account
  ( accountOperationH
  ) where

import           Control.Monad.Trans.Reader

import           Database.Persist.Sql
import           Servant

import           Models
import           Config

accountOperationH =
  getAccountH :<|> putAccountH :<|> deleteAccountH

getAccountH :: AccountId -> ReaderT App Handler (Maybe Account)
getAccountH accountId = do
  runDB $ get accountId

putAccountH :: AccountId -> Account -> ReaderT App Handler NoContent
putAccountH accountId account = do
  runDB $ repsert accountId account
  return NoContent

deleteAccountH :: AccountId -> ReaderT App Handler NoContent
deleteAccountH accountId = do
  runDB $ delete accountId
  return NoContent

