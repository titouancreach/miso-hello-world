{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import GHC.Generics (Generic)
import Miso
import Miso.Lens
import Miso.String (MisoString, ms)
import Network.Wai
import Prelude hiding (div)
import Servant
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified HeaderComponent as HC
import qualified Network.Wai.Handler.Warp as Warp

-- | Component model state
newtype Model = Model
  { _counter :: Int
  }
  deriving (Show, Eq)


counter :: Lens Model Int
counter = lens _counter $ \model new -> model { _counter = new }

updateModel :: Action -> Effect Model Action
updateModel NoOp = pure ()

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
  deriving (Show, Eq)

server :: Server API
server = pure (Page app)

main :: IO ()
main = do
  putStrLn "🚀 Running on http://localhost:3000"
  Warp.run 3000 $ serve (Proxy @API) server

app :: Component Model Action
app =
  component emptyModel updateModel viewModel

emptyModel :: Model
emptyModel = Model 0

viewModel :: Model -> View Action
viewModel x =
  div_
    []
    [ HC.viewModel ()
    ]