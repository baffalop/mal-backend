{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Text.IO as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  putStrLn $ "running on http://localhost:" <> show port
  Warp.run port $ websocketsOr WS.defaultConnectionOptions wsApp staticWithRoot

staticWithRoot :: Wai.Application
staticWithRoot req respond =
  case Wai.rawPathInfo req of
    "/" -> respond $ Wai.responseFile HTTP.status200 [("Content-Type", "text/html")] "public/index.html" Nothing
    _ -> staticApp req respond

staticApp :: Wai.Application
staticApp = Static.staticApp $ Static.defaultWebAppSettings publicDir

wsApp :: WS.ServerApp
wsApp pending = do
  con <- WS.acceptRequest pending
  putStrLn "Accepted new connection"
  WS.withPingThread con 30 (return ()) $ echo con

echo :: WS.Connection -> IO ()
echo con =
  forever $ do
    msg <- WS.receiveData con
    T.putStrLn $ "Message received: " <> msg
    WS.sendTextData con $ "Message received: " <> msg

port :: Int
port = 8080

publicDir :: String
publicDir = "public/"
