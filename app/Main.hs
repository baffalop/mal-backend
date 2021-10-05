{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import qualified World
import World (DownMsg(..), UpMsg(..))

port :: Int
port = 8080

publicDir :: String
publicDir = "public/"

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
  WS.withPingThread con 30 (return ()) $ world con

world :: WS.Connection -> IO ()
world con =
  forever $ do
    msg <- WS.receiveData con
    LB.putStrLn $ "Message received: " <> msg
    case Aeson.decode msg of
      Nothing -> do
        putStrLn "Invalid UpMsg"
      Just (Insert index newSpan) -> do
        putStrLn $ "Inserting new span at layer " <> show index
        WS.sendTextData con $ Aeson.encode $ Update $ World.insert newSpan index World.new
