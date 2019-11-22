{-# LANGUAGE OverloadedStrings #-}
module Reflex.DevServer
  ( devServer
  ) where

import Control.Concurrent.Async (concurrently_)
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.Warp (jsaddleOr, jsaddleJs')
import Network.HTTP.Types (status200, status400)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setPort, setTimeout)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, defaultConnectionOptions)

devServer :: Int -> Int -> Maybe FilePath -> JSM () -> Application -> ServerApp -> IO ()
devServer appPort wsPort mFile widget httpApp wsApp = do
  sequence_ $ flip writeFile "" <$> mFile
  concurrently_ jsaddleApp wsServer
  where
    jsaddleApp = runSettings (setPort appPort (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions (widget >> syncPoint) (app httpApp)
    wsServer = run wsPort (websocketsOr defaultConnectionOptions wsApp backupApp)
    backupApp _ res = res $ responseLBS status400 [] "Not a WebSocket request"

app :: Application -> Application
app mainApp req res = case (requestMethod req, pathInfo req) of
  ("GET", ["jsaddle.js"]) -> res $ responseLBS status200 [("Content-Type", "application/javascript")] $ jsaddleJs' Nothing False
  _ -> mainApp req res
