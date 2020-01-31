{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay ((?), Css, em)
import qualified Clay as C
import Development.Shake
import Lucid
import Path
import Relude
import qualified Rib
import Slug
import System.Environment
import Zulip.Client
import Data.Time
import Data.Time.Clock.POSIX

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
  Rib.buildStaticFiles [[relfile|static/**|]]
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

-- | Define your site HTML here
renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ case page of
      Page_Index _ -> "Fun Prog Zulip Archive"
      Page_Stream s -> toHtml $ _streamName s
      Page_StreamTopic _s t -> toHtml $ _topicName t
    style_ [type_ "text/css"] $ C.render pageStyle
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    stylesheet "https://fonts.googleapis.com/css?family=Open+Sans|Oswald&display=swap"
  body_ $ do
    with div_ [class_ "ui container", id_ "thesite"] $ do
      case page of
        Page_Index streams -> do
          let streamMsgCount stream = 
                length $ mconcat $ _topicMessages <$> fromMaybe [] (_streamTopics stream)
          with div_ [class_ "ui relaxed list"]
            $ forM_ (reverse $ sortOn streamMsgCount streams)
            $ \stream -> with div_ [class_ "item"] $ do
              with div_ [class_ "content"] $ do
                with a_ [class_ "header", href_ (streamUrl stream)]
                  $ toHtml
                  $ _streamName stream
                with div_ [class_ "description"] $ do
                  toHtml (_streamDescription stream)
                  div_ $ do 
                    toHtml $ show @Text $ streamMsgCount stream
                    " messages"
        Page_Stream stream -> do
          with h1_ [class_ "ui header"] $ toHtml $ _streamName stream
          p_ $ toHtml $ _streamDescription stream
          with div_ [class_ "ui relaxed list"]
            $ forM_ (fromMaybe [] $ _streamTopics stream)
            $ \topic -> with div_ [class_ "item"] $ do
              with div_ [class_ "content"] $ do
                with a_ [class_ "header", href_ ("/" <> topicUrl stream topic)]
                  $ toHtml
                  $ _topicName topic
                with div_ [class_ "description"] $ do
                  toHtml $ show @Text (length $ _topicMessages topic)
                  " messages."
                  " Last updated: "
                  toHtml $ maybe "" renderTimestamp $ _topicLastUpdated topic
        Page_StreamTopic stream topic -> do
          with h1_ [class_ "ui header"] $ do
            toHtml $ _streamName stream
            " > "
            toHtml $ _topicName topic
          with div_ [class_ "ui grid logs inverted segment"] $ do
            forM_ (_topicMessages topic) $ \msg -> do
              with div_ [class_ "row message top aligned"] $ do
                with div_ [class_ $ "three wide right aligned column timestamp"] $ do
                  let anchor = show $ _messageId msg
                  with a_ [name_ anchor, href_ $ "#" <> anchor] $ 
                    toHtml $ _messageSenderFullName msg
                  div_ $ renderTimestamp $ _messageTimestamp msg
                with div_ [class_ "thirteen wide column message-text"] $ 
                  toHtmlRaw $ _messageContent msg -- TODO: only if Html
  where
    renderTimestamp t = 
      toHtml $ formatTime defaultTimeLocale "%F %X" $ posixSecondsToUTCTime t
    stylesheet x = link_ [rel_ "stylesheet", href_ x]

-- | Define your site CSS here
pageStyle :: Css
pageStyle = "div#thesite" ? do
  C.marginTop $ em 1
  C.marginBottom $ em 1
  ".row.message" ? do 
    C.paddingTop $ em 0.5
    C.paddingBottom $ em 0
