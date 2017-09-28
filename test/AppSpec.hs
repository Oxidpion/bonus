{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module AppSpec where

import           Api
import           App

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except

import           Data.Text

import           Database.Persist.Sql
import           Models

import           Network.HTTP.Client
import           Network.Wai.Handler.Warp

import           Servant.API
import           Servant.Client

import           Test.Hspec
import           Test.Mockery.Directory

getAccount :: AccountId -> ClientM (Maybe Account)
putAccount :: AccountId -> Account -> ClientM NoContent
delAccount :: AccountId -> ClientM NoContent

(getAccount :<|> putAccount :<|> delAccount) = client api

spec :: Spec
spec = do
  around withApp $ do
    describe "GET /account/:id" $ do
      it "returns Nothing for non-existing accounts" $ \ port -> do
        let keyAccount = AccountKey 0
        try port (getAccount keyAccount) `shouldReturn` Nothing

    describe "PUT /account/:id" $ do
      it "allows to add a account" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account {accountName = "Zephir"}
        try port (putAccount keyAccount account) `shouldReturn` NoContent
        try port (getAccount keyAccount) `shouldReturn` Just account

      it "allows to add two accounts" $ \ port -> do
        let keyA = AccountKey 1
        let a = Account {accountName = "Zephir"}
        let keyB = AccountKey 2
        let b = Account {accountName = "Bill"}
        try port (putAccount keyA a)
        try port (putAccount keyB b)
        try port (getAccount keyB) `shouldReturn` Just b

      it "allows to update a account" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account {accountName = "Zephir"}
        let updateAccount = Account {accountName = "Zophit"}
        try port (putAccount keyAccount account)
        try port (putAccount keyAccount updateAccount)
        try port (getAccount keyAccount) `shouldReturn` Just updateAccount

    describe "DELETE /account/:id" $ do
      it "allows to delete a non-existing account" $ \ port -> do
        let keyAccount = AccountKey 0
        try port (delAccount keyAccount) `shouldReturn` NoContent

      it "allows to delete a account" $ \ port -> do
        let keyAccount = AccountKey 1
        let account = Account {accountName = "Zephir"}
        try port (delAccount keyAccount) `shouldReturn` NoContent
        try port (getAccount keyAccount) `shouldReturn` Nothing

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action

try :: Int -> ClientM a -> IO a
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Left err -> throwIO $ ErrorCall $ show err
    Right a -> return a
