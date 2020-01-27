{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Zulip.Client where

import Prelude hiding (Option)
import Data.Aeson
import Data.Aeson.TH
import Network.HTTP.Req
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

data Topics
  = Topics 
    { _topicsResult :: Text 
    , _topicsMsg :: Text 
    , _topicsTopics :: [Topic] 
    }
  deriving (Eq, Show)

data Topic 
  = Topic 
    { _topicName :: Text 
    , _topicMaxId :: Int 
    }
  deriving (Eq, Show)

$(deriveJSON fieldLabelMod ''Stream)
$(deriveJSON fieldLabelMod ''Streams)

$(deriveJSON fieldLabelMod ''Topics)
$(deriveJSON fieldLabelMod ''Topic)

type APIConfig scheme = (Url scheme, Option scheme)

demo :: MonadIO m => Text -> m [(Stream, [Topic])]
demo apiKey = do
  let auth = basicAuth "srid@srid.ca" $ encodeUtf8 apiKey
      apiConfig = (https baseUrl /: "api" /: "v1", auth)
  liftIO $ putStrLn "Running API request"
  runReq defaultHttpConfig $ do
    r <- getStreams apiConfig
    case r of
      Error s -> error $ toText s
      Success (v :: Streams) -> do 
        let streams = _streamsStreams v
        forM streams $ \stream -> do 
          getTopics apiConfig (_streamStreamId stream) >>= \case 
            Error s -> error $ toText s 
            Success topics -> do 
              pure (stream, topics)

getStreams :: MonadHttp m => APIConfig scheme -> m (Result Streams)
getStreams (apiUrl, auth) = do
  r <- req
    GET
    (apiUrl /: "streams")
    NoReqBody
    jsonResponse
    auth
  pure $ fromJSON (responseBody r :: Value)

getTopics :: MonadHttp m => APIConfig scheme -> Int -> m (Result [Topic])
getTopics (apiUrl, auth) streamId = do
  r <- req 
    GET
    (apiUrl /: "users" /: "me" /: show streamId /: "topics")
    NoReqBody 
    jsonResponse 
    auth 
  let res = _topicsTopics <$> fromJSON (responseBody r :: Value)
  pure res
