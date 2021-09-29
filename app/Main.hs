{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Text.IO as T
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS

main :: IO ()
main = do
  putStrLn $ "running on http://localhost:" <> show port <> "/"
  run port $ websocketsOr WS.defaultConnectionOptions wsApp httpApp

httpApp :: Wai.Application
httpApp _ send = send $ Wai.responseLBS HTTP.status200 [("Content-Type", "text/plain")] "Hello world!"

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
