{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module Model where

import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Miso
import Miso.Lens
import Miso.String (MisoString, ms)
import Network.URI
import Network.Wai
import Prelude hiding (div)
import Servant
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.Wai.Handler.Warp as Warp

data Model = Model
  { _counter :: Int,
    uri :: URI
  }
  deriving (Show, Eq)

counter :: Lens Model Int
counter = lens _counter $ \model new -> model {_counter = new}

updateModel :: Action -> Effect Model Action
updateModel NoOp = pure ()
updateModel (HandleURI u) = modify $ \m -> m {uri = u}
updateModel (ChangeURI u) = io_ $ do
    pushURI u
    consoleLog "Clicked" <> show u
    pure ()

newtype Page = Page (Component Model Action)

instance ToHtml Page where
  toHtml (Page x) =
    toHtml
      [ doctype_,
        head_
          []
          [ meta_ [charset_ "utf-8"],
            script_ [src_ "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"] ""
          ],
        body_ [] [toView x]
      ]

type API = Get '[HTML] Page

data Action
  = NoOp
  | HandleURI URI
  | ChangeURI URI
  deriving (Show, Eq)

type Routes =
       "login" :> View Action
  :<|> View Action

loginUri, homeUri :: URI
loginUri :<|> homeUri = allLinks' linkURI (Proxy @Routes)

goLogin :: Action
goLogin = ChangeURI loginUri

goHome :: Action
goHome = ChangeURI homeUri


emptyModel :: Model
emptyModel = Model 0 (fromJust (parseURI "http://localhost:3000"))
