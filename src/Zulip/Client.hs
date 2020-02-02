{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Zulip.Client where

import Crypto.Hash (Digest, MD5 (..), hash)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX
import Network.HTTP.Req
import Path
import Relude hiding (Option)
import Relude.Extra.Map (lookup)
import qualified Shower
import Slug
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
        _topicLastUpdated :: Maybe POSIXTime, -- Not in API; only used internally
        _topicSlug :: Path Rel File
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
        _messageAvatarUrl :: Maybe Text, -- API doesn't always set this.
        _messageSenderFullName :: Text,
        _messageSenderId :: Int,
        _messageStreamId :: Maybe Int,
        _messageSubject :: Text,
        _messageTimestamp :: POSIXTime,
        _messageType :: Text,
        _messageReactions :: [Reaction]
      }
  deriving (Eq, Show)

data Reaction
  = Reaction
      { _reactionEmojiCode :: Text,
        _reactionEmojiName :: Text,
        _reactionReactionType :: Text
      }
  deriving (Eq, Show)

data Users
  = Users
      { _usersMembers :: [User]
      }
  deriving (Eq, Show)

data User
  = User
      { _userAvatarUrl :: Text,
        _userId :: Int,
        _userFullName :: Text
      }
  deriving (Eq, Show)

-- | Make a non-injective function injective
mkInjective :: Ord b => [a] -> (a -> b) -> a -> (b, Maybe a)
mkInjective domain f a =
  let image = map f domain
      nonInjectiveImage = Set.fromList $ dups image
      b = f a
   in if Set.member b nonInjectiveImage
        then (b, Just a)
        else (b, Nothing)
  where
    dups = Map.keys . Map.filter (> 1) . Map.fromListWith (+) . fmap (,1 :: Int)

-- | Fill out hitherto missing _streamTopics, _topicMessages and _messageAvatarUrl
mkArchive :: [Stream] -> [User] -> [Message] -> [Stream]
mkArchive streams users msgsWithoutAvatar = flip fmap streams $ \stream ->
  -- TODO: Verify that stream names are unique.
  let avatarMap = Map.fromList $ flip fmap users $ _userId &&& _userAvatarUrl
      msgs = flip fmap msgsWithoutAvatar $ \msg ->
        msg {_messageAvatarUrl = _messageAvatarUrl msg <|> lookup (_messageSenderId msg) avatarMap}
      streamMsgs = flip filter msgs $ \msg -> _messageStreamId msg == Just (_streamStreamId stream)
      topicMsgMap = Map.fromListWith (<>) $ flip fmap streamMsgs $ \msg ->
        (_messageSubject msg, [msg])
   in stream
        { _streamTopics = Just (reverse $ sortOn _topicLastUpdated $ uncurry (mkTopic $ Map.keys topicMsgMap) <$> Map.toList topicMsgMap)
        }
  where
    mkTopic allTopics topicName ms =
      let tmsgs = sortOn _messageTimestamp ms
          mkTopicSlug = mkInjective allTopics mkSlugPure >>> \case
            (slug, Nothing) -> slug
            (slug, Just x) -> slug <> "-" <> textHash x
       in Topic topicName tmsgs (lastTimestamp tmsgs) (parseRelFilePure $ toString $ mkTopicSlug topicName)
    textHash s =
      let digest :: Digest MD5
          digest = hash $ encodeUtf8 @Text @ByteString s
       in show digest
    lastTimestamp xs =
      case reverse xs of
        msg : _ -> Just $ _messageTimestamp msg
        _ -> Nothing
    mkSlugPure =
      either (error . toText . displayException) unSlug . mkSlug
    parseRelFilePure =
      either (error . show) id . parseRelFile

$(deriveJSON fieldLabelMod ''Stream)

$(deriveJSON fieldLabelMod ''Streams)

$(deriveJSON fieldLabelMod ''Topics)

$(deriveJSON fieldLabelMod ''Topic)

$(deriveJSON fieldLabelMod ''Messages)

$(deriveJSON fieldLabelMod ''Message)

$(deriveJSON fieldLabelMod ''Reaction)

$(deriveJSON fieldLabelMod ''Users)

$(deriveJSON fieldLabelMod ''User)

type APIConfig scheme = (Url scheme, Option scheme)

getArchive :: MonadIO m => Text -> m [Stream]
getArchive apiKey = do
  let auth = basicAuth userEmail $ encodeUtf8 apiKey
      apiConfig = (https baseUrl /: "api" /: "v1", auth)
  runReq defaultHttpConfig $ do
    -- Fetch any remaining messages
    (hasNew, msgs) <- updateMessages apiConfig "messages.json"
    liftIO $ Shower.printer ("hasNew" :: Text, hasNew)
    -- Fetch streams
    streams <- getStreams apiConfig
    -- Fetch user avatars
    users <- getUsers apiConfig
    pure $ mkArchive streams users msgs

updateMessages :: (MonadIO m, MonadHttp m) => APIConfig scheme -> FilePath -> m (Bool, [Message])
updateMessages apiConfig messagesFile = do
  liftIO $ putStrLn $ "Loading " <> messagesFile
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

getStreams :: MonadHttp m => APIConfig scheme -> m [Stream]
getStreams (apiUrl, auth) =
  _streamsStreams . fromResult
    <$> apiGet auth (apiUrl /: "streams") NoReqBody mempty

getUsers :: MonadHttp m => APIConfig scheme -> m [User]
getUsers (apiUrl, auth) =
  _usersMembers . fromResult
    <$> apiGet auth (apiUrl /: "users") NoReqBody mempty

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

fromResult :: Result a -> a
fromResult = \case
  Error s -> error $ toText s
  Success v -> v
