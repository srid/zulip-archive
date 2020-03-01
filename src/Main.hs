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

import Clay ((?), Css, em, pct, px)
import qualified Clay as C
import qualified Config
import Config (Config)
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)
import Data.Some
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Development.Shake
import GHC.Natural
import Lucid
import Path
import Relude
import Rib (IsRoute (..))
import qualified Rib
import Text.HTML.TagSoup (maybeTagText, parseTags)
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
      pure [relfile|index.html|]
    Route_Stream stream r -> do
      streamSlug <- parseRelDir . toString . unSlug =<< mkSlug (_streamName stream)
      fmap (streamSlug </>) $ case r of
        StreamRoute_Index ->
          pure [relfile|index.html|]
        StreamRoute_Topic topic ->
          addExtension ".html" $ _topicSlug topic

main :: IO ()
main = prodMain

devMain :: IO ()
devMain = do
  cfg <- Config.readConfig
  -- Just dump development server's generated files under ./tmp for now.
  -- TODO: We need a configurable way to support development environments in rib.
  Rib.run [reldir|static|] [reldir|tmp|] $ generateSite cfg

prodMain :: IO ()
prodMain = forever $ do
  cfg <- Config.readConfig
  targetDir <- parseRelDir $ toString $ Config.targetDir cfg
  -- Run rib without a http server. Just generate *once*.
  Rib.runWith [reldir|static|] targetDir (generateSite cfg) (Rib.Generate False)
  putStrLn $ "Waiting for " <> show (Config.fetchEveryMins cfg) <> " min"
  threadDelayMins $ Config.fetchEveryMins cfg
  where
    threadDelayMins :: Natural -> IO ()
    threadDelayMins = threadDelay . (1000000 * 60 *) . naturalToInt

-- | Shake action for generating the static site
generateSite :: Config -> Action ()
generateSite cfg = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|**|]]
  -- Fetch (and/or load from cache) all zulip data
  (server, streams) <- getArchive (Config.zulipDomain cfg) (Config.authEmail cfg) (Config.authApiKey cfg)
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage server cfg r
  streamRoutes <- forM streams $ \stream -> do
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
    ogpMetaTags cfg route realmName
    title_ $ toHtml $ routeTitle realmName route
    style_ [type_ "text/css"] $ C.render routeStyle
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    stylesheet "https://fonts.googleapis.com/css?family=Open+Sans|Oswald&display=swap"
    googleFonts $ [headerFont, bodyFont]
  body_ $ do
    div_ [class_ "ui text container", id_ "thesite"] $ do
      div_ [class_ "ui violet inverted top attached center aligned segment"] $ do
        h1_ [class_ "ui huge header"] $ toHtml @Text $ case route of
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
  where
    renderPageContent :: Html ()
    renderPageContent = case route of
      Route_Index -> do
        let streams = reverse $ flip sortOn val $ \(Route_Stream stream StreamRoute_Index) -> streamMsgCount stream
            streamMsgCount :: Stream -> Int
            streamMsgCount stream =
              length $ mconcat $ _topicMessages <$> fromMaybe [] (_streamTopics stream)
        div_ [class_ "ui relaxed list"]
          $ forM_ streams
          $ \sr -> div_ [class_ "item"] $ do
            let Route_Stream stream StreamRoute_Index = sr
            div_ [class_ "right floated content subtle"] $
              case streamMsgCount stream of
                0 -> mempty
                cnt -> do
                  toHtml $ show @Text cnt
                  " messages"
            div_ [class_ "content"] $ do
              a_ [class_ "header", href_ $ Rib.routeUrl sr]
                $ toHtml
                $ _streamName stream
              div_ [class_ "description"] $ do
                toHtml (_streamDescription stream)
      Route_Stream stream StreamRoute_Index -> do
        let mkTopicRoute s t = Route_Stream s $ StreamRoute_Topic t
        p_ $ i_ $ toHtml $ _streamDescription stream
        div_ [class_ "ui relaxed list"]
          $ forM_ (fromMaybe [] $ _streamTopics stream)
          $ \topic -> div_ [class_ "item"] $ do
            div_ [class_ "right floated content subtle"] $ do
              toHtml $ maybe "" renderTimestamp $ _topicLastUpdated topic
            div_ [class_ "content"] $ do
              a_ [class_ "header", href_ $ Rib.routeUrl $ mkTopicRoute stream topic]
                $ toHtml
                $ _topicName topic
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
                  Just avatarUrl -> img_ [src_ avatarUrl]
              div_ [class_ "content"] $ do
                a_ [class_ "author"] $ toHtml $ _messageSenderFullName msg
                div_ [class_ "metadata"] $ do
                  let anchor = show $ _messageId msg
                  a_ [name_ anchor, href_ $ "#" <> anchor]
                    $ div_
                    $ renderTimestamp
                    $ _messageTimestamp msg
                div_ [class_ "text"] $
                  toHtmlRaw (_messageContent msg)
    renderTimestamp t =
      toHtml $ formatTime defaultTimeLocale "%F %X" $ posixSecondsToUTCTime t
    stylesheet x = link_ [rel_ "stylesheet", href_ x]
    googleFonts fs =
      let s = T.intercalate "|" $ T.replace " " "+" <$> fs
          url = "https://fonts.googleapis.com/css?family=" <> s <> "&display=swap"
       in stylesheet url

ogpMetaTags :: Config -> Route a -> Text -> Html ()
ogpMetaTags cfg r realmName = do
  let ogpAttribute name value =
        meta_ [term "property" $ "og:" <> name, content_ value]
  ogpAttribute "site_name" $ realmName <> " Archive"
  ogpAttribute "url" $ Config.baseUrl cfg <> Rib.routeUrl r
  ogpAttribute "title" $ routeTitle realmName r
  case r of
    Route_Index ->
      mempty
    Route_Stream stream StreamRoute_Index -> do
      ogpAttribute "description" $ _streamDescription stream
    Route_Stream stream (StreamRoute_Topic topic) -> do
      ogpAttribute "type" "article"
      ogpAttribute "article:section" $ _streamName stream
      mapM_ (ogpAttribute "article:modified_time" . ogpTimeFormat) $
        _topicLastUpdated topic
      whenJust (listToMaybe $ _topicMessages topic) $ \msg -> do
        ogpAttribute "article:published_time" $ ogpTimeFormat $ _messageTimestamp msg
        ogpAttribute "description" $ T.take 300 $ stripHtml $ _messageContent msg
        ogpAttribute "image" `mapM_` _messageAvatarUrl msg
  where
    stripHtml :: Text -> Text
    stripHtml = T.concat . mapMaybe maybeTagText . parseTags
    ogpTimeFormat :: POSIXTime -> Text
    ogpTimeFormat =
      toText
        . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")
        . posixSecondsToUTCTime

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
routeCrumbs = (Some Route_Index :|) . \case
  Route_Index ->
    []
  r@(Route_Stream _ StreamRoute_Index) ->
    [Some r]
  r@(Route_Stream stream (StreamRoute_Topic _)) ->
    [ Some $ Route_Stream stream StreamRoute_Index,
      Some r
    ]

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
routeStyle = "div#thesite" ? do
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
      forM_ [C.h1, C.h2, C.h3, C.h4, C.h5, C.h6, ".ui.breadcrumb.rib .section", ".header", ".as-header"] $ \h -> h ? do
        C.fontFamily [headerFont] [C.sansSerif]
        C.lineHeight $ em 1.2
      "code, pre, tt"
        ? C.fontFamily ["SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [C.monospace]
      -- Get rid of the font lock in text container
      C.important $ C.fontSize $ em 1
