{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay (Css, em, pct, px, (?))
import qualified Clay as C
import Config (Config)
import qualified Config
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Data.Some
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Development.Shake
import GHC.Natural
import Lucid
import Relude
import Rib (IsRoute (..))
import qualified Rib
import Rib.Extra.CSS (googleFonts, stylesheet)
import Rib.Extra.OpenGraph (Article (..), OGType (..), OpenGraph (..))
import System.FilePath
import System.IO (BufferMode (LineBuffering), hSetBuffering)
import System.Posix.Files (touchFile)
import Text.HTML.TagSoup (maybeTagText, parseTags)
import qualified Text.URI as URI
import Web.Slug (mkSlug, unSlug)
import Zulip.Client

-- | Sentinel types for GADTs
data StreamR = StreamR

data TopicR = TopicR

-- | Route represents the route for each generated static page.
data Route a where
  Route_Index :: Route [Route StreamR]
  Route_Stream :: Stream -> StreamRoute a -> Route a

data StreamRoute a where
  StreamRoute_Index :: StreamRoute StreamR
  StreamRoute_Topic :: Topic -> StreamRoute TopicR

instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure "index.html"
    Route_Stream stream r -> do
      streamSlug <- fmap (toString . unSlug) $ mkSlug (_streamName stream)
      fmap (streamSlug </>) $ case r of
        StreamRoute_Index ->
          pure "index.html"
        StreamRoute_Topic topic ->
          pure $ flip addExtension ".html" $ _topicSlug topic

main :: IO ()
main = do
  let inputDir = "static"
      outputDir = "site"
  cfg <- Config.readConfig
  putTextLn $ "Running zulip-archive for: " <> Config.zulipDomain cfg
  let gen = Rib.run inputDir outputDir $ generateSite cfg
      freq = Config.fetchEveryMins cfg
  if freq < 1
    then gen
    else do
      race_
        gen
        do
          scheduleGeneration inputDir freq

-- | Periodically trigger site generation
--
-- For every `Config.fetchEveryMins`, this effectively induces rib to invoke
-- `generateSite`
scheduleGeneration :: FilePath -> Natural -> IO ()
scheduleGeneration inputDir freq = do
  hSetBuffering stdout LineBuffering
  forever $ do
    putStrLn $ "Waiting for " <> show freq <> " min"
    threadDelayMins freq
    -- Modify a file under the input directory so that Rib runs site generation.
    touchFile $ inputDir </> ".rib-trigger"
  where
    threadDelayMins :: Natural -> IO ()
    threadDelayMins = threadDelay . (1000000 * 60 *) . naturalToInt

-- | Shake action for generating the static site
generateSite :: Config -> Action ()
generateSite cfg = do
  -- Fetch (and/or load from cache) all zulip data
  (server, streams) <- getArchive (Config.zulipDomain cfg) (Config.authEmail cfg) (Config.authApiKey cfg)
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage server cfg r
  streamRoutes <- forM streams $ \stream -> unless (_streamName stream == "#Random") $ do
    forM_ (fromMaybe (error "No topics in stream") $ _streamTopics stream) $ \topic -> do
      let tr = Route_Stream stream $ StreamRoute_Topic topic
      writeHtmlRoute tr TopicR
    let sr = Route_Stream stream StreamRoute_Index
    writeHtmlRoute sr StreamR
    pure sr
  -- Write an index.html linking to all streams
  writeHtmlRoute Route_Index streamRoutes

-- | Define your site HTML here
renderPage :: ServerSettings -> Config -> Route a -> a -> Html ()
renderPage server cfg route val = html_ [lang_ "en"] $ do
  let realmName = _serversettingsRealmName server <> " Zulip"
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    toHtml $ routeOgpMeta cfg realmName route
    title_ $ toHtml $ routeTitle realmName route
    style_ [type_ "text/css"] $ C.render routeStyle
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    stylesheet "https://fonts.googleapis.com/css?family=Open+Sans|Oswald&display=swap"
    googleFonts $ [headerFont, bodyFont]
  body_ $ do
    div_ [class_ "ui text container", id_ "thesite"] $ do
      div_ [class_ "ui violet inverted top attached center aligned segment"] $ do
        h1_ [class_ "ui huge header"] $
          toHtml @Text $ case route of
            Route_Index ->
              realmName <> " Chat Archive"
            Route_Stream stream StreamRoute_Index ->
              _streamName stream <> " stream"
            Route_Stream stream (StreamRoute_Topic topic) ->
              _topicName topic <> " - " <> _streamName stream
      div_ [class_ "ui attached segment"] $ do
        div_ [class_ "ui message"] $ do
          p_ $ do
            "Welcome to the "
            toHtml $ realmName
            " Chat Archive. You can join the chat "
            a_ [href_ $ _serversettingsRealmUri server] "here"
            "."
        renderCrumbs $ routeCrumbs route
        renderPageContent
      div_ [class_ "ui vertical footer segment"] $ do
        a_ [href_ "https://github.com/srid/zulip-archive"] "Powered by Haskell"
        " | Questions? Contact "
        a_ [href_ "https://www.srid.ca"] "srid"
  where
    renderPageContent :: Html ()
    renderPageContent = case route of
      Route_Index -> do
        let streams = reverse $ flip sortOn val $ \(Route_Stream stream StreamRoute_Index) -> streamMsgCount stream
            streamMsgCount :: Stream -> Int
            streamMsgCount stream =
              length $ mconcat $ _topicMessages <$> fromMaybe [] (_streamTopics stream)
        div_ [class_ "ui relaxed list"] $
          forM_ streams $
            \sr -> div_ [class_ "item"] $ do
              let Route_Stream stream StreamRoute_Index = sr
              div_ [class_ "right floated content subtle"] $
                case streamMsgCount stream of
                  0 -> mempty
                  cnt -> do
                    toHtml $ show @Text cnt
                    " messages"
              div_ [class_ "content"] $ do
                a_ [class_ "header", href_ $ Rib.routeUrl sr] $
                  toHtml $
                    _streamName stream
                div_ [class_ "description"] $ do
                  toHtml (_streamDescription stream)
      Route_Stream stream StreamRoute_Index -> do
        let mkTopicRoute s t = Route_Stream s $ StreamRoute_Topic t
        p_ $ i_ $ toHtml $ _streamDescription stream
        div_ [class_ "ui relaxed list"] $
          forM_ (fromMaybe [] $ _streamTopics stream) $
            \topic -> div_ [class_ "item"] $ do
              div_ [class_ "right floated content subtle"] $ do
                toHtml $ maybe "" renderTimestamp $ _topicLastUpdated topic
              div_ [class_ "content"] $ do
                a_ [class_ "header", href_ $ Rib.routeUrl $ mkTopicRoute stream topic] $
                  toHtml $
                    _topicName topic
                div_ [class_ "description"] $ do
                  toHtml $ show @Text (length $ _topicMessages topic)
                  " messages."
      Route_Stream _ (StreamRoute_Topic topic) -> do
        div_ [class_ "ui comments messages"] $ do
          forM_ (_topicMessages topic) $ \msg -> do
            div_ [class_ "comment"] $ do
              a_ [class_ "avatar"] $ do
                case _messageAvatarUrl msg of
                  Nothing -> mempty
                  Just avatarUrl -> img_ [src_ $ URI.render avatarUrl]
              div_ [class_ "content"] $ do
                a_ [class_ "author"] $ toHtml $ _messageSenderFullName msg
                div_ [class_ "metadata"] $ do
                  let anchor = show $ _messageId msg
                  a_ [name_ anchor, href_ $ "#" <> anchor] $
                    div_ $
                      renderTimestamp $
                        _messageTimestamp msg
                div_ [class_ "text"] $
                  toHtmlRaw (_messageContent msg)
    renderTimestamp t =
      toHtml $ formatTime defaultTimeLocale "%F %X" $ posixSecondsToUTCTime t

routeTitle :: Text -> Route a -> Text
routeTitle realmName = \case
  Route_Index -> realmName <> " Archive"
  Route_Stream s StreamRoute_Index ->
    _streamName s <> " - " <> realmName
  Route_Stream stream (StreamRoute_Topic topic) ->
    _topicName topic <> " - " <> _streamName stream

routeName :: Route a -> Text
routeName = \case
  Route_Index -> "Home"
  Route_Stream stream StreamRoute_Index ->
    "#" <> _streamName stream
  Route_Stream _ (StreamRoute_Topic topic) ->
    _topicName topic

routeCrumbs :: Route a -> NonEmpty (Some Route)
routeCrumbs =
  (Some Route_Index :|) . \case
    Route_Index ->
      []
    r@(Route_Stream _ StreamRoute_Index) ->
      [Some r]
    r@(Route_Stream stream (StreamRoute_Topic _)) ->
      [ Some $ Route_Stream stream StreamRoute_Index,
        Some r
      ]

routeOgpMeta :: Config -> Text -> Route a -> OpenGraph
routeOgpMeta cfg realmName route =
  OpenGraph
    { _openGraph_title = routeTitle realmName route,
      _openGraph_url = URI.mkURI $ Config.baseUrl cfg <> Rib.routeUrl route,
      _openGraph_author = Nothing,
      _openGraph_description = description,
      _openGraph_siteName = realmName <> " Archive",
      _openGraph_type = ogType,
      _openGraph_image = case route of
        Route_Stream _ (StreamRoute_Topic topic) ->
          _messageAvatarUrl =<< firstMessage topic
        _ ->
          Nothing
    }
  where
    description :: Maybe Text
    description = case route of
      Route_Index ->
        Nothing
      Route_Stream stream StreamRoute_Index ->
        Just $ _streamDescription stream
      Route_Stream _ (StreamRoute_Topic topic) ->
        T.take 300 . stripHtml . _messageContent <$> firstMessage topic
    ogType :: Maybe OGType
    ogType = case route of
      Route_Stream stream (StreamRoute_Topic topic) ->
        Just $
          OGType_Article $
            Article
              { _article_section =
                  Just $ _streamName stream,
                _article_modifiedTime =
                  posixSecondsToUTCTime <$> _topicLastUpdated topic,
                _article_publishedTime =
                  posixSecondsToUTCTime . _messageTimestamp <$> firstMessage topic,
                _article_expirationTime = Nothing,
                _article_tag = []
              }
      _ ->
        Nothing
    firstMessage = listToMaybe . _topicMessages
    stripHtml :: Text -> Text
    stripHtml = T.concat . mapMaybe maybeTagText . parseTags

renderCrumbs :: NonEmpty (Some Route) -> Html ()
renderCrumbs (_ :| []) = mempty
renderCrumbs crumbs =
  div_ [class_ "ui breadcrumb rib"] $ go crumbs
  where
    go :: NonEmpty (Some Route) -> Html ()
    go (p0 :| []) = do
      div_ [class_ "active section"] $ toHtml $ routeName `foldSome` p0
    go (p0 :| p1 : ps) = do
      a_ [class_ "section", href_ $ Rib.routeUrl `foldSome` p0] $ toHtml $ routeName `foldSome` p0
      i_ [class_ "right angle icon divider"] mempty
      go $ p1 :| ps

headerFont :: Text
headerFont = "Roboto"

bodyFont :: Text
bodyFont = "Open Sans"

-- | Define your site CSS here
routeStyle :: Css
routeStyle =
  "div#thesite" ? do
    baseStyle
    C.marginTop $ em 1
    C.marginBottom $ em 1
    ".ui.breadcrumb.rib" ? do
      C.marginBottom $ em 1
    ".ui.comments.messages" ? do
      ".comment" ? do
        ".metadata a" ? do
          C.color C.grey
        "a.avatar img" ? do
          C.height C.auto -- Fix vertical stretching of avatar
    ".subtle" ? do
      C.color C.grey
    ".messages" ? do
      "pre" ? do
        C.fontSize $ pct 85
        C.overflow C.auto
        C.maxWidth $ pct 100
      ".message_embed" ? do
        C.borderLeft C.solid (px 3) C.grey
        C.paddingLeft $ em 0.7
      ".message_inline_image img" ? do
        C.maxWidth $ pct 100
        C.marginBottom $ em 1
  where
    baseStyle :: Css
    baseStyle = do
      -- Fonts
      C.fontFamily [bodyFont] [C.sansSerif]
      forM_ [C.h1, C.h2, C.h3, C.h4, C.h5, C.h6, ".ui.breadcrumb.rib .section", ".header", ".as-header"] $ \h ->
        h ? do
          C.fontFamily [headerFont] [C.sansSerif]
          C.lineHeight $ em 1.2
      "code, pre, tt"
        ? C.fontFamily ["SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [C.monospace]
      -- Get rid of the font lock in text container
      C.important $ C.fontSize $ em 1
