{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Utils.NeqMap (NeqMap)
import qualified Utils.NeqMap as NeqMap
import qualified World
import World (DownMsg(..), UpMsg(..), World)
import Data.Function ((&))

port :: Int
port = 8080

publicDir :: String
publicDir = "frontend/dist/"

main :: IO ()
main = do
  putStrLn $ "running on http://localhost:" <> show port
  world <- newMVar World.new
  clients <- newMVar (NeqMap.empty :: NeqMap WS.Connection)
  Warp.run port $ websocketsOr WS.defaultConnectionOptions (wsApp clients world) staticWithRoot

staticWithRoot :: Wai.Application
staticWithRoot req respond =
  case Wai.rawPathInfo req of
    "/" -> respond $ Wai.responseFile HTTP.status200 [("Content-Type", "text/html")] (publicDir <> "index.html") Nothing
    _ -> staticApp req respond

staticApp :: Wai.Application
staticApp = Static.staticApp $ Static.defaultWebAppSettings publicDir

wsApp :: MVar (NeqMap WS.Connection) -> MVar World -> WS.ServerApp
wsApp vCons vWorld pending = do
  con <- WS.acceptRequest pending
  putStrLn "Accepted new connection"

  -- Establish world for new client
  readMVar vWorld >>= WS.sendTextData con . Aeson.encode . Update

  -- Manage connections
  index <- modifyMVar vCons $ pure . NeqMap.insert con
  let disconnect = do
      putStrLn "Disconnected"
      modifyMVar_ vCons $ pure . NeqMap.delete index

  worldApp vCons con vWorld
    & keepAlive con 
    & (`finally` disconnect)

worldApp :: MVar (NeqMap WS.Connection) -> WS.Connection -> MVar World -> IO ()
worldApp vCons con vWorld =
  forever $ do
    msg <- WS.receiveData con
    LB.putStrLn $ "Message received: " <> msg
    case Aeson.decode msg of
      Nothing -> do
        LB.putStrLn $ "Invalid UpMsg: " <> msg
        WS.sendTextData con $ Aeson.encode $ Error "Couldn't parse up msg"

      Just (Insert index newSpan) -> do
        putStrLn $ "Inserting new span at layer " <> show index
        newWorld <- modifyReturnMVar vWorld $ World.insert newSpan index
        broadcast vCons $ Update newWorld

broadcast :: MVar (NeqMap WS.Connection) -> DownMsg -> IO ()
broadcast vCons msg = do
  connections <- NeqMap.toList <$> readMVar vCons
  forM_ connections $ flip WS.sendTextData $ Aeson.encode msg

keepAlive :: WS.Connection -> IO () -> IO ()
keepAlive con = WS.withPingThread con 30 (return ())

modifyReturnMVar :: MVar a -> (a -> a) -> IO a
modifyReturnMVar var f = modifyMVar var $ pure . dup . f

dup :: a -> (a, a)
dup x = (x, x)