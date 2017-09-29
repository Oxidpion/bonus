{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Handler.Account
  ( accountOperationH
  , Balance
  , Income
  , Expense
  ) where

import           Data.Aeson
import           Data.Time.Clock
import           Data.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class

import           Database.Persist.Sql
import           Servant

import           Models
import           Config

accountOperationH =
  getAccountH :<|> putAccountH :<|> deleteAccountH :<|> balanceAccountH :<|> incomeAccountH :<|> expenseAccountH

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


newtype Balance = Balance Int

instance ToJSON Balance where
  toJSON (Balance x) = object ["balance" .= x ]

instance FromJSON Balance where
  parseJSON = withObject "Balance" $ \o ->
    Balance <$> o .: "balance"

balanceAccountH :: AccountId -> ReaderT App Handler Balance
balanceAccountH accountId = do
  account <- runDB $ get accountId
  case account of
    (Just _) -> do
      let sql = "SELECT SUM(change) as balance FROM \"transaction\" WHERE account_id = ? GROUP BY account_id"
      bonuses <- runDB $ rawSql sql [toPersistValue accountId]
      return $ Balance $ maybe 0 unSingle (listToMaybe bonuses)
    Nothing -> return $ Balance 0

class ToTransaction a where
    toTransaction :: AccountId -> a -> IO Transaction

data Income = Income
  { incomeDeferredSecond :: Int
  , incomeChange :: Int
  }

instance ToJSON Income where
  toJSON (Income s c) = object
    [ "deferred_second" .= s
    , "change"          .= c
    ]

instance FromJSON Income where
  parseJSON = withObject "Income" $ \o -> do
    incomeDeferredSecond  <- o .: "deferred_second"
    incomeChange          <- o .: "change"
    return Income {..}

instance ToTransaction Income where
  toTransaction accountId (Income s c) = do
    let True = c > 0
    time <- getCurrentTime
    return Transaction
      { transactionAccountId  = accountId
      , transactionChange     = c
      , transactionDate       = addUTCTime (realToFrac s) time
      }

incomeAccountH :: AccountId -> Income -> ReaderT App Handler (Maybe Transaction)
incomeAccountH accountId income = do
  account <- runDB $ get accountId
  transaction <- liftIO $ toTransaction accountId income
  case account of
    (Just _) -> do
      runDB $ insert transaction
      return $ Just transaction
    Nothing -> return Nothing


data Expense = Expense
  { expenseDeferredSecond :: Int
  , expenseChange :: Int
  }

instance ToJSON Expense where
  toJSON (Expense s c) = object
    [ "deferred_second" .= s
    , "change"          .= c
    ]

instance FromJSON Expense where
  parseJSON = withObject "Expense" $ \o -> do
    expenseDeferredSecond <- o .: "deferred_second"
    expenseChange         <- o .: "change"
    return Expense {..}

instance ToTransaction Expense where
  toTransaction accountId (Expense s c) = do
    let True = c > 0
    time <- getCurrentTime
    return Transaction
      { transactionAccountId  = accountId
      , transactionChange     = negate c
      , transactionDate       = addUTCTime (realToFrac s) time
      }

expenseAccountH :: AccountId -> Expense -> ReaderT App Handler (Maybe Transaction)
expenseAccountH accountId expense = do
  account <- runDB $ get accountId
  transaction <- liftIO $ toTransaction accountId expense
  case account of
    (Just _) -> do
      runDB $ insert transaction
      return $ Just transaction
    Nothing -> return Nothing
