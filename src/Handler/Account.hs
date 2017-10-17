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
  , balance
  , Income
  , income
  , Expense
  , expense
  ) where

import           Data.Aeson
import           Data.Time.Clock
import           Data.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Control.Monad (when)

import           Database.Persist.Sql
import           Servant

import           Models
import           Config

accountOperationH =
  getAccountH :<|> putAccountH :<|> deleteAccountH :<|> balanceAccountH :<|> incomeAccountH :<|> expenseAccountH

getAccountH :: AccountId -> ReaderT App Handler Account
getAccountH accountId = do
  maybeAccount <- runDB $ get accountId
  maybe (throwError $ err404 { errBody = "Account not found" }) return maybeAccount

putAccountH :: AccountId -> Account -> ReaderT App Handler NoContent
putAccountH accountId account = do
  runDB $ repsert accountId account
  return NoContent

deleteAccountH :: AccountId -> ReaderT App Handler NoContent
deleteAccountH accountId = do
  runDB $ delete accountId
  return NoContent


newtype Balance = Balance Int deriving (Eq, Show)

balance :: Int -> Balance
balance = Balance

instance ToJSON Balance where
  toJSON (Balance x) = object ["balance" .= x ]

instance FromJSON Balance where
  parseJSON = withObject "Balance" $ \o ->
    Balance <$> o .: "balance"

balanceAccountH :: AccountId -> ReaderT App Handler Balance
balanceAccountH accountId = do
  account <- getAccountH accountId
  let sql = "SELECT SUM(change) as balance FROM \"transaction\" WHERE account_id = ? GROUP BY account_id"
  bonuses <- runDB $ rawSql sql [toPersistValue accountId]
  return $ Balance $ maybe 0 unSingle (listToMaybe bonuses)

class ToTransaction a where
    toTransaction :: AccountId -> a -> IO Transaction

data Income = Income
  { incomeDeferredSecond :: Int
  , incomeChange :: Int
  }

income :: Int -> Int -> Income
income deferredSeconds change = Income
  { incomeDeferredSecond = deferredSeconds
  , incomeChange = change
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
    time <- getCurrentTime
    return Transaction
      { transactionAccountId  = accountId
      , transactionChange     = c
      , transactionDate       = addUTCTime (realToFrac s) time
      }

incomeAccountH :: AccountId -> Income -> ReaderT App Handler Transaction
incomeAccountH accountId income = do
  when (incomeChange income < 0) (throwError $ err400 { errBody = "Change will be >0" })
  account <- getAccountH accountId
  transaction <- liftIO $ toTransaction accountId income
  runDB $ insert transaction
  return transaction


data Expense = Expense
  { expenseDeferredSecond :: Int
  , expenseChange :: Int
  }

expense :: Int -> Int -> Expense
expense deferredSecond change = Expense
  { expenseDeferredSecond = deferredSecond
  , expenseChange = change
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
    time <- getCurrentTime
    return Transaction
      { transactionAccountId  = accountId
      , transactionChange     = negate c
      , transactionDate       = addUTCTime (realToFrac s) time
      }

expenseAccountH :: AccountId -> Expense -> ReaderT App Handler Transaction
expenseAccountH accountId expense = do
  when (expenseChange expense < 0) (throwError $ err400 { errBody = "Change will be >0" } )
  account <- getAccountH accountId
  transaction <- liftIO $ toTransaction accountId expense
  runDB $ insert transaction
  return transaction
