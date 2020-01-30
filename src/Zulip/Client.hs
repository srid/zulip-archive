{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Zulip.Client where

import Data.Aeson
import Data.Aeson.TH
import Network.HTTP.Req
import Relude hiding (Option)
import qualified Shower
import Zulip.Internal

baseUrl :: Text
baseUrl = "funprog.zulipchat.com"

userEmail :: ByteString
userEmail = "srid@srid.ca"

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
      { _topicsResult :: Text,
        _topicsMsg :: Text,
        _topicsTopics :: [Topic]
      }
  deriving (Eq, Show)

data Topic
  = Topic
      { _topicName :: Text,
        _topicMaxId :: Int
      }
  deriving (Eq, Show)

data Messages
  = Messages
      { _messagesAnchor :: Int,
        _messagesFoundAnchor :: Bool,
        _messagesFoundNewest :: Bool,
        _messagesMessages :: [Message]
      }
  deriving (Eq, Show)

data Message
  = Message
      { _messageId :: Int,
        _messageContent :: Text,
        _messageContentType :: Text,
        _messageSenderId :: Int,
        _messageSenderEmail :: Text,
        _messageSenderFullName :: Text,
        _messageSenderShortName :: Text,
        _messageStreamId :: Int,
        _messageSubject :: Text,
        _messageTimestamp :: Int, -- UTCTime,
        _messageType :: Text
      }
  deriving (Eq, Show)

data Narrow
  = Narrow
      { _narrowOperator :: Text,
        _narrowOperand :: Text,
        _narrowNegated :: Bool
      }
  deriving (Eq, Show)

narrowStream :: Text -> [Narrow]
narrowStream name = [Narrow "stream" name False]

$(deriveJSON fieldLabelMod ''Stream)

$(deriveJSON fieldLabelMod ''Streams)

$(deriveJSON fieldLabelMod ''Topics)

$(deriveJSON fieldLabelMod ''Topic)

$(deriveJSON fieldLabelMod ''Messages)

$(deriveJSON fieldLabelMod ''Message)

$(deriveJSON fieldLabelMod ''Narrow)

type APIConfig scheme = (Url scheme, Option scheme)

-- | Just a playground for now.
demo :: MonadIO m => Text -> m [(Stream, [Topic])]
demo apiKey = do
  let auth = basicAuth userEmail $ encodeUtf8 apiKey
      apiConfig = (https baseUrl /: "api" /: "v1", auth)
  liftIO $ putStrLn "Running API request"
  runReq defaultHttpConfig $ do
    streams <- getStreams apiConfig >>= \case
      Error s -> error $ toText s
      Success (v :: Streams) -> do
        let streams = take 2 $ _streamsStreams v -- WIP (take 2)
        forM streams $ \stream -> do
          getTopics apiConfig (_streamStreamId stream) >>= \case
            Error s -> error $ toText s
            Success topics -> do
              pure (stream, topics)
    -- Play with messages API
    getMessages apiConfig 0 10 [] >>= \case
      Error s -> error $ toText s
      Success msgs -> do
        liftIO $ Shower.printer msgs
        liftIO $ encodeFile "messages.json" $ _messagesMessages msgs
    pure streams

getStreams :: MonadHttp m => APIConfig scheme -> m (Result Streams)
getStreams (apiUrl, auth) = do
  apiGet auth (apiUrl /: "streams") NoReqBody mempty

getTopics :: forall m scheme. MonadHttp m => APIConfig scheme -> Int -> m (Result [Topic])
getTopics (apiUrl, auth) streamId = do
  fmap _topicsTopics <$> apiGet auth (apiUrl /: "users" /: "me" /: show streamId /: "topics") NoReqBody mempty

getMessages :: MonadHttp m => APIConfig scheme -> Int -> Int -> [Narrow] -> m (Result Messages)
getMessages (apiUrl, auth) anchor numAfter narrow = do
  let narrowText :: Text = decodeUtf8 $ encode narrow
      payload =
        mconcat
          [ "anchor" =: anchor,
            "num_before" =: (4 :: Int),
            "num_after" =: numAfter,
            "narrow" =: narrowText
          ]
  apiGet auth (apiUrl /: "messages") NoReqBody payload

apiGet ::
  (MonadHttp m, HttpBody body, HttpBodyAllowed (AllowsBody GET) (ProvidesBody body), FromJSON a) =>
  Option scheme ->
  Url scheme ->
  body ->
  Option scheme ->
  m (Result a)
apiGet auth url reqBody opts = do
  liftIO $ putStrLn $ "↓ " <> show url
  r <-
    req
      GET
      url
      reqBody
      jsonResponse
      (auth <> opts)
  pure $ fromJSON (responseBody r :: Value)
