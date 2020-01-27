{-# LANGUAGE TemplateHaskell #-}

module Zulip.Client where

import Data.Aeson
import Data.Aeson.TH
import Network.HTTP.Req
import qualified Shower
import Zulip.Internal

baseUrl :: Text
baseUrl = "funprog.zulipchat.com"

data Streams
  = Streams
      { _streamsResult :: Text,
        _streamsMsg :: Text,
        _streamsStreams :: [Stream]
      }
  deriving (Eq, Show)

data Stream
  = Stream
      { _streamName :: Text,
        _streamDescription :: Text,
        _streamStreamId :: Int
      }
  deriving (Eq, Show)

$(deriveJSON fieldLabelMod ''Stream)

$(deriveJSON fieldLabelMod ''Streams)

demo :: MonadIO m => Text -> m [Stream]
demo apiKey = do
  let auth = basicAuth "srid@srid.ca" $ encodeUtf8 apiKey
  r <- runReq defaultHttpConfig $ do
    req
      GET
      (https baseUrl /: "api" /: "v1" /: "streams")
      NoReqBody
      jsonResponse
      auth
  case fromJSON (responseBody r :: Value) of
    Error s -> error $ toText s
    Success (v :: Streams) -> do 
      liftIO $ Shower.printer v
      pure $ _streamsStreams v
