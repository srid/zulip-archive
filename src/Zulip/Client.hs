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
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import Network.HTTP.Req
import Relude hiding (Option)
import Relude.Extra.Map (lookup)
import qualified Shower
import System.Directory (doesFileExist)
import Text.URI (URI, mkURI)
import Web.UniqSlug (mkUniqSlug)
import Zulip.Internal

data Streams = Streams
  { _streamsResult :: Text,
    _streamsMsg :: Text,
    _streamsStreams :: [Stream]
  }
  deriving (Eq, Show)

data Stream = Stream
  { _streamName :: Text,
    _streamDescription :: Text,
    _streamStreamId :: Int,
    _streamTopics :: Maybe [Topic] -- Not in API; only used internally
  }
  deriving (Eq, Show)

data Topics = Topics
  { _topicsResult :: Text,
    _topicsMsg :: Text,
    _topicsTopics :: [Topic]
  }
  deriving (Eq, Show)

data Topic = Topic
  { _topicName :: Text,
    _topicMessages :: [Message], -- Not in API; only used internally
    _topicLastUpdated :: Maybe POSIXTime, -- Not in API; only used internally
    _topicSlug :: FilePath
  }
  deriving (Eq, Show)

data Messages = Messages
  { _messagesAnchor :: Int,
    _messagesFoundAnchor :: Bool,
    _messagesFoundNewest :: Bool,
    _messagesMessages :: [Message]
  }
  deriving (Eq, Show)

data Message = Message
  { _messageId :: Int,
    _messageContent :: Text,
    _messageContentType :: Text,
    _messageAvatarUrl :: Maybe URI, -- API doesn't always set this.
    _messageGravatarUrl :: Maybe URI, -- Not in API; only used internally
    _messageSenderFullName :: Text,
    _messageSenderId :: Int,
    _messageStreamId :: Maybe Int,
    _messageSubject :: Text,
    _messageTimestamp :: POSIXTime,
    _messageType :: Text,
    _messageReactions :: [Reaction]
  }
  deriving (Eq, Show)

data Reaction = Reaction
  { _reactionEmojiCode :: Text,
    _reactionEmojiName :: Text,
    _reactionReactionType :: Text
  }
  deriving (Eq, Show)

data Users = Users
  { _usersMembers :: [User]
  }
  deriving (Eq, Show)

data User = User
  { _userAvatarUrl :: Maybe Text,
    _userUserId :: Int,
    _userFullName :: Text,
    _userEmail :: Text
  }
  deriving (Eq, Show)

data ServerSettings = ServerSettings
  { _serversettingsRealmIcon :: Text,
    _serversettingsRealmName :: Text,
    _serversettingsRealmDescription :: Text, -- HTML
    _serversettingsRealmUri :: Text
  }
  deriving (Eq, Show)

-- | Fill out hitherto missing _streamTopics, _topicMessages and _messageAvatarUrl
mkArchive :: [Stream] -> [User] -> [Message] -> [Stream]
mkArchive streams users msgsWithoutAvatar = flip fmap streams $ \stream ->
  -- TODO: Verify that stream names are unique.
  let avatarMap = Map.fromList $ flip mapMaybe users $ \u -> (_userUserId u,) <$> _userAvatarUrl u
      emailMap = Map.fromList $ users <&> \u -> (_userUserId u, _userEmail u)
      msgs = flip fmap msgsWithoutAvatar $ \msg ->
        msg
          { _messageAvatarUrl =
              _messageAvatarUrl msg <|> (lookup (_messageSenderId msg) avatarMap >>= mkURI)
          , _messageGravatarUrl =
                _messageGravatarUrl msg <|> (lookup (_messageSenderId msg) emailMap >>= mkGravatarURI)
          }
      streamMsgs = flip filter msgs $ \msg -> _messageStreamId msg == Just (_streamStreamId stream)
      topicMsgMap = Map.fromListWith (<>) $
        flip fmap streamMsgs $ \msg ->
          (_messageSubject msg, [msg])
   in stream
        { _streamTopics = Just (reverse $ sortOn _topicLastUpdated $ uncurry (mkTopic $ Map.keys topicMsgMap) <$> Map.toList topicMsgMap)
        }
  where
    mkTopic allTopics topicName ms =
      let tmsgs = sortOn _messageTimestamp ms
          mkTopicSlug = mkUniqSlug allTopics
       in Topic topicName tmsgs (lastTimestamp tmsgs) (toString $ mkTopicSlug topicName)
    lastTimestamp xs =
      case reverse xs of
        msg : _ -> Just $ _messageTimestamp msg
        _ -> Nothing
    mkGravatarURI :: Text -> Maybe URI
    mkGravatarURI email =
      mkURI $ "https://www.gravatar.com/avatar/" <> T.pack (show $ md5 $ encodeUtf8 email)

type APIConfig scheme = (Url scheme, Option scheme)

getArchive :: MonadIO m => Text -> Text -> Text -> m (ServerSettings, [Stream])
getArchive baseUrl userEmail apiKey = do
  let auth = basicAuth (fromString $ toString userEmail) $ encodeUtf8 apiKey
      apiConfig = (https baseUrl /: "api" /: "v1", auth)
  runReq defaultHttpConfig $ do
    -- Fetch any remaining messages
    (hasNew, msgs) <- updateMessages apiConfig "messages.json"
    liftIO $ Shower.printer ("hasNew" :: Text, hasNew)
    -- Fetch streams
    streams <- getStreams apiConfig
    -- Fetch user avatars
    users <- getUsers apiConfig
    -- Fetch metadata
    serverSettings <- getServerSettings apiConfig
    pure $ (serverSettings, mkArchive streams users msgs)

updateMessages :: (MonadIO m, MonadHttp m) => APIConfig scheme -> FilePath -> m (Bool, [Message])
updateMessages apiConfig messagesFile = do
  liftIO $ putStrLn $ "Loading " <> messagesFile
  savedMsgs <-
    liftIO (doesFileExist messagesFile) >>= \case
      False -> pure []
      True ->
        liftIO (eitherDecodeFileStrict messagesFile) >>= \case
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

getServerSettings :: MonadHttp m => APIConfig scheme -> m ServerSettings
getServerSettings (apiUrl, auth) =
  fromResult <$> apiGet auth (apiUrl /: "server_settings") NoReqBody mempty

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

$(deriveJSON fieldLabelMod ''Stream)

$(deriveJSON fieldLabelMod ''Streams)

$(deriveJSON fieldLabelMod ''Topics)

$(deriveJSON fieldLabelMod ''Topic)

$(deriveJSON fieldLabelMod ''Messages)

$(deriveJSON fieldLabelMod ''Message)

$(deriveJSON fieldLabelMod ''Reaction)

$(deriveJSON fieldLabelMod ''Users)

$(deriveJSON fieldLabelMod ''User)

$(deriveJSON fieldLabelMod ''ServerSettings)
