{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module AppSpec where

import           Api
import           App
import           Config
import           Handler.Account

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except

import           Data.Text

import           Database.Persist.Sql
import           Models

import           Network.HTTP.Client
import           Network.Wai.Handler.Warp

import           Servant.API
import           Servant.Client as Client

import           Test.Hspec
import           Test.Mockery.Directory

getAccount :: AccountId -> ClientM Account
putAccount :: AccountId -> Account -> ClientM NoContent
delAccount :: AccountId -> ClientM NoContent
balanceAccount :: AccountId -> ClientM Balance
incomeAccount :: AccountId -> Income -> ClientM Transaction
expenseAccount :: AccountId -> Expense -> ClientM Transaction

(getAccount :<|> putAccount :<|> delAccount :<|> balanceAccount :<|> incomeAccount :<|> expenseAccount) = client api

spec :: Spec
spec = do
  around withApp $ do
    describe "GET /account/:id" $ do
      it "reject to get non-existing accounts" $ \ port -> do
        let keyAccount = AccountKey 0
        try port (getAccount keyAccount) `shouldThrow` anyErrorCall

    describe "PUT /account/:id" $ do
      it "allows to add a account" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Zephir" }
        try port (putAccount keyAccount account) `shouldReturn` NoContent
        try port (getAccount keyAccount) `shouldReturn` account

      it "allows to add two accounts" $ \ port -> do
        let keyA = AccountKey 1
        let a = Account { accountName = "Zephir" }
        let keyB = AccountKey 2
        let b = Account { accountName = "Bill" }
        try port (putAccount keyA a)
        try port (putAccount keyB b)
        try port (getAccount keyB) `shouldReturn` b

      it "allows to update a account" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Zephir" }
        let updateAccount = Account { accountName = "Zophit" }
        try port (putAccount keyAccount account)
        try port (putAccount keyAccount updateAccount)
        try port (getAccount keyAccount) `shouldReturn` updateAccount

    describe "DELETE /account/:id" $ do
      it "allows to delete a non-existing account" $ \ port -> do
        let keyAccount = AccountKey 0
        try port (delAccount keyAccount) `shouldReturn` NoContent

      it "allows to delete a account" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Zephir" }
        try port (delAccount keyAccount) `shouldReturn` NoContent
        try port (getAccount keyAccount) `shouldThrow` anyErrorCall

    describe "GET /account/:id/balance" $ do
      it "reject to get balance for non-existing accounts" $ \ port -> do
        let keyAccount = AccountKey 0
        try port (balanceAccount keyAccount) `shouldThrow` anyErrorCall

      it "allows to get balance a account without transaction" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Alice" }
        try port (putAccount keyAccount account)
        try port (balanceAccount keyAccount) `shouldReturn` balance 0

      it "allows to get balance a account with two transaction" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Alice" }
        try port (putAccount keyAccount account)
        let incomeT = income 0 100
        try port (incomeAccount keyAccount incomeT)
        let expenseT = expense 0 11
        try port (expenseAccount keyAccount expenseT)
        try port (balanceAccount keyAccount) `shouldReturn` balance 89

    describe "POST /account/:id/income" $ do
      it "reject to process income for non-existing accounts" $ \ port -> do
        let keyAccount = AccountKey 0
        let incomeT = income 0 100
        try port (incomeAccount keyAccount incomeT) `shouldThrow` anyErrorCall

      it "reject to process income for change a income < 0" $ \ port -> do
        let keyAccount = AccountKey 0
        let account = Account { accountName = "Bruce" }
        try port (putAccount keyAccount account)
        let incomeT = income 0 (-100)
        try port (incomeAccount keyAccount incomeT) `shouldThrow` anyErrorCall

      it "allows to accrual a account with two income transaction" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Bruce" }
        try port (putAccount keyAccount account)
        let incomeT = income 0 99
        try port (incomeAccount keyAccount incomeT)
        try port (incomeAccount keyAccount incomeT)
        try port (balanceAccount keyAccount) `shouldReturn` balance 198

    describe "POST /account/:id/expense" $ do
      it "reject to process expense for non-existing accounts" $ \ port -> do
        let keyAccount = AccountKey 0
        let expenseT = expense 0 100
        try port (expenseAccount keyAccount expenseT) `shouldThrow` anyErrorCall

      it "reject to process for change a expense < 0" $ \ port -> do
        let keyAccount = AccountKey 0
        let account = Account { accountName = "Samara" }
        try port (putAccount keyAccount account)
        let expenseT = expense 0 (-10)
        try port (expenseAccount keyAccount expenseT) `shouldThrow` anyErrorCall

      it "allows to use a expense a account two transaction" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account { accountName = "Samara" }
        try port (putAccount keyAccount account)
        let expenseT = expense 0 10
        try port (expenseAccount keyAccount expenseT)
        try port (expenseAccount keyAccount expenseT)
        try port (balanceAccount keyAccount) `shouldReturn` balance (-20)


withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp
    runSqlPool (runMigration migrateAll) (appConnPool app)
    testWithApplication (return $ warpApplication app) action

try :: Int -> ClientM a -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a -> return a
