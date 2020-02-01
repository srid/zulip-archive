{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay ((?), Css, em, pct, px)
import qualified Clay as C
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Development.Shake
import Lucid
import Path
import Relude
import qualified Rib
import Slug
import System.Environment
import Zulip.Client

data Page
  = Page_Index [Stream]
  | Page_Stream Stream
  | Page_StreamTopic Stream Topic

main :: IO ()
main = Rib.runWith [reldir|a|] [reldir|b|] generateSite (Rib.Serve 8080 False)

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|user_uploads/**|]]
  [apiKey] <- fmap toText <$> liftIO getArgs
  streams <- getArchive apiKey
  forM_ streams $ \stream -> do
    f <- liftIO $ parseRelFile $ toString $ streamHtmlPath stream
    Rib.writeHtml f $ renderPage $ Page_Stream stream
    case _streamTopics stream of
      Nothing -> error "No topics stored in stream"
      Just topics -> forM_ topics $ \topic -> do
        g <- liftIO $ parseRelFile $ toString $ topicHtmlPath stream topic
        Rib.writeHtml g $ renderPage $ Page_StreamTopic stream topic
  -- Write an index.html linking to the aforementioned files.
  Rib.writeHtml [relfile|index.html|] $
    renderPage (Page_Index streams)

streamHtmlPath :: Stream -> Text
streamHtmlPath stream = streamUrl stream <> "index.html"

topicHtmlPath :: Stream -> Topic -> Text
topicHtmlPath = topicUrl

topicUrl :: Stream -> Topic -> Text
topicUrl stream topic = either (error . toText . displayException) id $ do
  topicSlug <- mkSlug $ _topicName topic
  pure $ streamUrl stream <> unSlug topicSlug <> ".html"

streamUrl :: Stream -> Text
streamUrl stream = either (error . toText . displayException) id $ do
  streamSlug <- mkSlug $ _streamName stream
  pure $ unSlug streamSlug <> "/"

renderCrumbs :: NonEmpty Page -> Html ()
renderCrumbs (_ :| []) = mempty
renderCrumbs crumbs =
  with div_ [class_ "ui breadcrumb rib"] $ go crumbs
  where
    go :: NonEmpty Page -> Html ()
    go (p0 :| []) = do
      with div_ [class_ "active section"] $ toHtml $ pageName p0
    go (p0 :| p1 : ps) = do
      with a_ [class_ "section", href_ $ pageUrl p0] $ toHtml $ pageName p0
      with i_ [class_ "right angle icon divider"] mempty
      go $ p1 :| ps

pageName :: Page -> Text
pageName = \case
  Page_Index _ -> "Home"
  Page_Stream s -> "#" <> _streamName s
  Page_StreamTopic _s t -> _topicName t

pageUrl :: Page -> Text
pageUrl = \case
  Page_Index _ -> "/"
  Page_Stream s -> "/" <> streamUrl s
  Page_StreamTopic s t -> "/" <> topicUrl s t

pageCrumbs :: Page -> NonEmpty Page
pageCrumbs = (Page_Index [] :|) . \case
  Page_Index _ -> []
  x@(Page_Stream _) -> [x]
  x@(Page_StreamTopic s _) -> [Page_Stream s, x]

-- | Define your site HTML here
renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ $ case page of
      Page_Index _ -> "Fun Prog Zulip Archive"
      Page_Stream s -> do
        toHtml $ _streamName s
        " - Fun Prog Zulip Archive"
      Page_StreamTopic s t -> do
        toHtml $ _topicName t
        " - "
        toHtml $ _streamName s
    style_ [type_ "text/css"] $ C.render pageStyle
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    stylesheet "https://fonts.googleapis.com/css?family=Open+Sans|Oswald&display=swap"
    googleFonts $ [headerFont, bodyFont]
  body_ $ do
    with div_ [class_ "ui text container", id_ "thesite"] $ do
      with div_ [class_ "ui violet inverted top attached center aligned segment"]
        $ with h1_ [class_ "ui huge header"]
        $ pageTitle page
      with div_ [class_ "ui attached segment"] $ do
        renderCrumbs $ pageCrumbs page
        case page of
          Page_Index streams -> do
            let streamMsgCount stream =
                  length $ mconcat $ _topicMessages <$> fromMaybe [] (_streamTopics stream)
            with div_ [class_ "ui raised segment"] $ do
              p_ $ do
                "Welcome to the Functional Programming Zulip Chat Archive. You can join the chat "
                with a_ [href_ "https://funprog.zulipchat.com/"] "here"
                "."
            with div_ [class_ "ui relaxed list"]
              $ forM_ (reverse $ sortOn streamMsgCount streams)
              $ \stream -> with div_ [class_ "item"] $ do
                with div_ [class_ "right floated content subtle"] $
                  case streamMsgCount stream of
                    0 -> mempty
                    cnt -> do
                      toHtml $ show @Text cnt
                      " messages"
                with div_ [class_ "content"] $ do
                  with a_ [class_ "header", href_ (streamUrl stream)]
                    $ toHtml
                    $ _streamName stream
                  with div_ [class_ "description"] $ do
                    toHtml (_streamDescription stream)
          Page_Stream stream -> do
            p_ $ i_ $ toHtml $ _streamDescription stream
            with div_ [class_ "ui relaxed list"]
              $ forM_ (fromMaybe [] $ _streamTopics stream)
              $ \topic -> with div_ [class_ "item"] $ do
                with div_ [class_ "right floated content subtle"] $ do
                  toHtml $ maybe "" renderTimestamp $ _topicLastUpdated topic
                with div_ [class_ "content"] $ do
                  with a_ [class_ "header", href_ ("/" <> topicUrl stream topic)]
                    $ toHtml
                    $ _topicName topic
                  with div_ [class_ "description"] $ do
                    toHtml $ show @Text (length $ _topicMessages topic)
                    " messages."
          Page_StreamTopic _stream topic -> do
            with div_ [class_ "ui comments messages"] $ do
              forM_ (_topicMessages topic) $ \msg -> do
                with div_ [class_ "comment"] $ do
                  with a_ [class_ "avatar"] $ do
                    case _messageAvatarUrl msg of
                      Nothing -> mempty
                      Just avatarUrl -> img_ [src_ avatarUrl]
                  with div_ [class_ "content"] $ do
                    with a_ [class_ "author"] $ toHtml $ _messageSenderFullName msg
                    with div_ [class_ "metadata"] $ do
                      let anchor = show $ _messageId msg
                      with a_ [name_ anchor, href_ $ "#" <> anchor]
                        $ div_
                        $ renderTimestamp
                        $ _messageTimestamp msg
                    with div_ [class_ "text"] $
                      toHtmlRaw (_messageContent msg)
      with div_ [class_ "ui vertical footer segment"] $ do
        with a_ [href_ "https://github.com/srid/zulip-archive"] "Powered by Haskell"
  where
    renderTimestamp t =
      toHtml $ formatTime defaultTimeLocale "%F %X" $ posixSecondsToUTCTime t
    pageTitle = toHtml . \case
      Page_Index _ -> "Functional Programming Zulip Chat Archive"
      Page_Stream s -> _streamName s <> " stream"
      Page_StreamTopic s t -> _topicName t <> " - " <> _streamName s
    stylesheet x = link_ [rel_ "stylesheet", href_ x]
    googleFonts fs =
      let s = T.intercalate "|" $ T.replace " " "+" <$> fs
          url = "https://fonts.googleapis.com/css?family=" <> s <> "&display=swap"
       in stylesheet url

headerFont :: Text
headerFont = "Roboto"

bodyFont :: Text
bodyFont = "Open Sans"

-- | Define your site CSS here
pageStyle :: Css
pageStyle = "div#thesite" ? do
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
