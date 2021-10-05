{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((&&&))
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
import qualified World
import World (DownMsg(..), UpMsg(..), World)

port :: Int
port = 8080

publicDir :: String
publicDir = "public/"

main :: IO ()
main = do
  putStrLn $ "running on http://localhost:" <> show port
  world <- newMVar World.new
  clients <- newMVar ([] :: [WS.Connection])
  Warp.run port $ websocketsOr WS.defaultConnectionOptions (wsApp clients world) staticWithRoot

staticWithRoot :: Wai.Application
staticWithRoot req respond =
  case Wai.rawPathInfo req of
    "/" -> respond $ Wai.responseFile HTTP.status200 [("Content-Type", "text/html")] "public/index.html" Nothing
    _ -> staticApp req respond

staticApp :: Wai.Application
staticApp = Static.staticApp $ Static.defaultWebAppSettings publicDir

wsApp :: MVar [WS.Connection] -> MVar World -> WS.ServerApp
wsApp cons world pending = do
  con <- WS.acceptRequest pending
  putStrLn "Accepted new connection"
  index <- modifyMVar cons $ pure . ((<> [con]) &&& length)
  let disconnect = putStrLn "Disconnected" >> modifyMVar_ cons (pure . withoutIndex index)
  (keepAlive con $ worldApp cons con world) `finally` disconnect

worldApp :: MVar [WS.Connection] -> WS.Connection -> MVar World -> IO ()
worldApp cons con world =
  forever $ do
    msg <- WS.receiveData con
    LB.putStrLn $ "Message received: " <> msg
    case Aeson.decode msg of
      Nothing -> do
        putStrLn "Invalid UpMsg"
      Just (Insert index newSpan) -> do
        putStrLn $ "Inserting new span at layer " <> show index
        newWorld <- modifyReturnMVar world $ World.insert newSpan index
        broadcast cons $ Update newWorld

broadcast :: MVar [WS.Connection] -> DownMsg -> IO ()
broadcast cons msg = do
  connections <- readMVar cons
  forM_ connections $ flip WS.sendTextData $ Aeson.encode msg

keepAlive :: WS.Connection -> IO () -> IO ()
keepAlive con = WS.withPingThread con 30 (return ())

modifyReturnMVar :: MVar a -> (a -> a) -> IO a
modifyReturnMVar var f = modifyMVar var $ pure . dup . f

dup :: a -> (a, a)
dup x = (x, x)

withoutIndex :: Int -> [a] -> [a]
withoutIndex index list = take index list ++ drop (index + 1) list
