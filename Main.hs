{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where


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
import qualified HeaderComponent as HC
import qualified Network.Wai.Handler.Warp as Warp
import Model


main :: IO ()
main = do
  putStrLn "🚀 Running on http://localhost:3000"
  Warp.run 3000 $ serve (Proxy @API) server

app :: Component Model Action
app =
  (component emptyModel updateModel viewModel)
    { subs = [uriSub HandleURI]
    }

viewModel :: Model -> View Action
viewModel m = view_
  where
    view_ = either (const notFound) id $ Miso.route (Proxy @Routes) handlers uri m
    handlers = loginPage :<|> homePage

    homePage (_ :: Model) = div_
      []
      [ HC.viewModel ()
      ]

    loginPage (m :: Model) = div_ [] [text "Login"]

    notFound = div_ [] [text "not found"]


server :: Server API
server = pure (Page app)