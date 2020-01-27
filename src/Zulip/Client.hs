module Zulip.Client where

import Data.Aeson (Value)
import Network.HTTP.Req

baseUrl :: Text
baseUrl = "funprog.zulipchat.com"

demo :: Text -> IO ()
demo apiKey = do
  r <- runReq defaultHttpConfig $ do
    req
      GET
      (https baseUrl /: "api" /: "v1" /: "streams")
      NoReqBody
      jsonResponse
      (basicAuth "srid@srid.ca" $ encodeUtf8 apiKey)
  print (responseBody r :: Value)
