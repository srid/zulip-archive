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
import qualified Data.Map.Strict as Map
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
        _streamTopics :: Maybe [Topic] -- Not in API; only used internally
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
        _topicMessages :: [Message], -- Not in API; only used internally
        _topicLastUpdated :: Maybe POSIXTime
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
        _messageAvatarUrl :: Maybe Text,  -- FIXME: api doesn't return it
        _messageSenderFullName :: Text,
        _messageStreamId :: Maybe Int,
        _messageSubject :: Text,
        _messageTimestamp :: POSIXTime,
        _messageType :: Text
      }
  deriving (Eq, Show)

-- | Fill out hitherto missing _streamTopics and _topicMessages
mkArchive :: [Stream] -> [Message] -> [Stream]
mkArchive streams msgs = flip fmap streams $ \stream ->
  -- TODO: Verify that stream and topic names are unique.
  let streamMsgs = flip filter msgs $ \msg -> _messageStreamId msg == Just (_streamStreamId stream)
      topicMsgMap = Map.fromListWith (<>) $ flip fmap streamMsgs $ \msg ->
        (_messageSubject msg, [msg])
   in stream
        { _streamTopics = Just (reverse $ sortOn _topicLastUpdated $ uncurry mkTopic <$> Map.toList topicMsgMap)
        }
  where
    mkTopic topicName ms =
      let tmsgs = sortOn _messageTimestamp ms
       in Topic topicName tmsgs (lastTimestamp tmsgs)
    lastTimestamp xs =
      case reverse xs of
        msg : _ -> Just $ _messageTimestamp msg
        _ -> Nothing

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
      -- TODO: use EitherT
      Error s -> error $ toText s
      Success v -> pure $ _streamsStreams v
    -- Fetch remaining messages
    (hasNew, msgs) <- updateMessages apiConfig "messages.json"
    liftIO $ Shower.printer ("hasNew" :: Text, hasNew)
    pure $ mkArchive streams msgs

updateMessages :: (MonadIO m, MonadHttp m) => APIConfig scheme -> FilePath -> m (Bool, [Message])
updateMessages apiConfig messagesFile = do
  savedMsgs <- liftIO (doesFileExist messagesFile) >>= \case
    False -> pure []
    True -> liftIO (eitherDecodeFileStrict messagesFile) >>= \case
      Left e -> error $ toText e
      Right (msgs :: [Message]) ->
        pure msgs
  let savedMsgsSplit = unconsRev savedMsgs
      lastMsgId = maybe 0 (_messageId . fst) savedMsgsSplit
      savedMsgsRest = maybe [] snd savedMsgsSplit
  liftIO $ Shower.printer ("lastMsgId" :: Text, lastMsgId)
  newMsgs <- fetchMessages apiConfig lastMsgId 1000
  let msgs = savedMsgsRest <> newMsgs
  liftIO $ encodeFile messagesFile msgs
  liftIO $ Shower.printer ("Writing " :: Text, length savedMsgsRest, " <> " :: Text, length newMsgs)
  pure (msgs /= savedMsgs, msgs)
  where
    unconsRev :: [a] -> Maybe (a, [a])
    unconsRev = fmap (fmap reverse) . uncons . reverse

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
