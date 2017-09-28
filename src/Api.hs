{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (api, Api) where

import Data.Proxy
import Data.Text

import Models

import Servant.API


type Api =
       "account" :> Capture "id" AccountId :> Get '[JSON] (Maybe Account)
  :<|> "account" :> Capture "id" AccountId :> ReqBody '[JSON] Account :> Put '[JSON] NoContent
  :<|> "account" :> Capture "id" AccountId :> DeleteNoContent '[JSON] NoContent

api :: Proxy Api
api = Proxy
