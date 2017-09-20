{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (api, Api) where

import Data.Proxy
import Data.Text

import Database.Persist

import Models

import Servant.API


type AccountOperationApi =
       Get '[JSON] (Maybe Account)
  :<|> ReqBody '[JSON] Account :> Put '[JSON] NoContent
  :<|> DeleteNoContent '[JSON] NoContent

type Api =
       "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
  :<|> "account" :> Capture "id" AccountId :> AccountOperationApi

api :: Proxy Api
api = Proxy
