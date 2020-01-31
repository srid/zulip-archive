{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Zulip.Client where

import Data.Aeson
import Data.Aeson.TH
import Data.Time.Clock.POSIX
import Network.HTTP.Req
import Relude hiding (Option)
import qualified Shower
import System.Directory (doesFileExist)
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
        _streamStreamId :: Int,
        _streamTopics :: Maybe [Topic]  -- Not in API; only used internally
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
        _topicMaxId :: Int,
        _topicMessages :: Maybe [Message]  -- Not in API; only used internally
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
        _messageStreamId :: Maybe Int,
        _messageSubject :: Text,
        _messageTimestamp :: POSIXTime,
        _messageType :: Text
      }
  deriving (Eq, Show)

-- | Fill out hitherto missing _streamTopics and _topicMessages
mkArchive :: [(Stream, [Topic])] -> [Message] -> [Stream] 
mkArchive streams msgs = flip fmap streams $ \(stream, topics) -> 
  stream { _streamTopics = Just (flip fmap topics $ \topic -> 
    topic { _topicMessages = Just $ filterTopicMessages stream topic msgs })
         }
  where
    -- | Get messages under the given stream's topic
    filterTopicMessages stream topic = filter $ \msg ->
      and
        [ _messageStreamId msg == Just (_streamStreamId stream),
          _messageSubject msg == _topicName topic
        ]

$(deriveJSON fieldLabelMod ''Stream)

$(deriveJSON fieldLabelMod ''Streams)

$(deriveJSON fieldLabelMod ''Topics)

$(deriveJSON fieldLabelMod ''Topic)

$(deriveJSON fieldLabelMod ''Messages)

$(deriveJSON fieldLabelMod ''Message)

type APIConfig scheme = (Url scheme, Option scheme)

getArchive :: MonadIO m => Text -> m [Stream]
getArchive apiKey = do
  let auth = basicAuth userEmail $ encodeUtf8 apiKey
      apiConfig = (https baseUrl /: "api" /: "v1", auth)
  liftIO $ putStrLn "Running API request"
  runReq defaultHttpConfig $ do
    -- Fetch streams and topics
    streams <- getStreams apiConfig >>= \case
      Error s -> error $ toText s
      Success (v :: Streams) -> do
        let streams = _streamsStreams v
        forM streams $ \stream -> do
          getTopics apiConfig (_streamStreamId stream) >>= \case
            Error s -> error $ toText s
            Success topics -> do
              pure (stream, topics)
    -- Fetch remaining messages
    let messagesFile = "messages.json"
    (savedMsgs, lastMsgId) <- liftIO (doesFileExist messagesFile) >>= \case
      False -> pure ([], 0)
      True -> liftIO (eitherDecodeFileStrict messagesFile) >>= \case
        Left e -> error $ toText e
        Right (msgs :: [Message]) ->
          case reverse msgs of
            [] -> pure ([], 0)
            (msg : rest) -> pure (reverse rest, _messageId msg)
    liftIO $ Shower.printer ("lastMsgId" :: Text, lastMsgId)
    newMsgs <- fetchMessages apiConfig lastMsgId 1000
    let msgs = savedMsgs <> newMsgs
    liftIO $ encodeFile messagesFile msgs
    liftIO $ Shower.printer ("Writing " :: Text, length savedMsgs, " <> " :: Text, length newMsgs)
    pure $ mkArchive streams msgs

fetchMessages :: MonadHttp m => APIConfig scheme -> Int -> Int -> m [Message]
fetchMessages apiConfig lastMsgId num = do
  getMessages apiConfig lastMsgId num >>= \case
    Error s -> error $ toText s
    Success newMsgs -> do
      let msgs = _messagesMessages newMsgs
      liftIO $ Shower.printer ("Fetched " :: Text, length msgs, _messagesFoundAnchor newMsgs, _messagesFoundNewest newMsgs)
      case (_messagesFoundNewest newMsgs, reverse msgs) of
        -- Fetch more if available
        (False, (lastMsg : _)) -> (msgs <>) <$> fetchMessages apiConfig (_messageId lastMsg) num
        _ -> pure msgs

getStreams :: MonadHttp m => APIConfig scheme -> m (Result Streams)
getStreams (apiUrl, auth) = do
  apiGet auth (apiUrl /: "streams") NoReqBody mempty

getTopics :: forall m scheme. MonadHttp m => APIConfig scheme -> Int -> m (Result [Topic])
getTopics (apiUrl, auth) streamId = do
  fmap _topicsTopics <$> apiGet auth (apiUrl /: "users" /: "me" /: show streamId /: "topics") NoReqBody mempty

getMessages :: MonadHttp m => APIConfig scheme -> Int -> Int -> m (Result Messages)
getMessages (apiUrl, auth) anchor numAfter = do
  let payload =
        mconcat
          [ "anchor" =: anchor,
            "num_before" =: (0 :: Int),
            "num_after" =: numAfter
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
