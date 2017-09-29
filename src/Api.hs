{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (api, Api) where

import Data.Proxy
import Data.Text

import Models
import Handler.Account

import Servant.API


type Api =
       "account" :> Capture "id" AccountId :> Get '[JSON] (Maybe Account)
  :<|> "account" :> Capture "id" AccountId :> ReqBody '[JSON] Account :> Put '[JSON] NoContent
  :<|> "account" :> Capture "id" AccountId :> DeleteNoContent '[JSON] NoContent
  :<|> "account" :> Capture "id" AccountId :> "balance" :> Get '[JSON] Balance
  :<|> "account" :> Capture "id" AccountId :> "income" :> ReqBody '[JSON] Income :> Post '[JSON] (Maybe Transaction)
  :<|> "account" :> Capture "id" AccountId :> "expense" :> ReqBody '[JSON] Expense :> Post '[JSON] (Maybe Transaction)

api :: Proxy Api
api = Proxy
