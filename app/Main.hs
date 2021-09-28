{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn $ "running on http://localhost:" <> show port <> "/"
  run port app

app :: Wai.Application
app _ send = send $ Wai.responseLBS HTTP.status200 [("Content-Type", "text/plain")] "Hello world!"

port :: Int
port = 8080
