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


type AccountAPI =
       "account" :> Capture "id" AccountId :> Get '[JSON] (Maybe Account)
  :<|> "account" :> Capture "id" AccountId :> ReqBody '[JSON] Account :> Put '[JSON] NoContent
  :<|> "account" :> Capture "id" AccountId :> DeleteNoContent '[JSON] NoContent

type Api =
       "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
  :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
  :<|> AccountAPI

api :: Proxy Api
api = Proxy
