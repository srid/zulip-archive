{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Clay ((?), Css, em, pc, px, sym)
import qualified Clay as C
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Development.Shake
import Lucid
import Path
import Rib (MMark, Source)
import qualified Rib
import qualified Rib.Parser.MMark as MMark
import System.Environment
import Zulip.Client

data Page
  = Page_Index [Stream]
  | Page_Stream Stream

main :: IO ()
main = Rib.runWith [reldir|a|] [reldir|b|] generateSite (Rib.Serve 8080 False)

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  [apiKey] <- fmap toText <$> liftIO getArgs
  streams <- demo apiKey
  forM_ streams $ \stream -> do
    f <- liftIO $ parseRelFile $ streamHtmlPath stream
    Rib.writeHtml f $ renderPage $ Page_Stream stream
  -- Write an index.html linking to the aforementioned files.
  Rib.writeHtml [relfile|index.html|] $
    renderPage (Page_Index streams)

streamHtmlPath :: (Semigroup s, IsString s) => Stream -> s
streamHtmlPath stream = show (_streamStreamId stream) <> ".html"

-- | Define your site HTML here
renderPage :: Page -> Html ()
renderPage page = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ case page of
      Page_Index _ -> "Fun Prog Zulip Archive"
      Page_Stream s -> toHtml $ _streamName s
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    with div_ [id_ "thesite"] $ do
      with div_ [class_ "header"] $
        with a_ [href_ "/"] "Back to Home"
      case page of
        Page_Index streams -> div_ $ forM_ streams $ \stream ->
          with li_ [class_ "pages"] $ do
            b_ $ with a_ [href_ (streamHtmlPath stream)] $ toHtml $ _streamName stream
        Page_Stream stream -> do
          h1_ $ toHtml $ _streamName stream
          p_ $ toHtml $ _streamDescription stream
  where
    _renderMarkdown =
      MMark.render . either error id . MMark.parsePure "<none>"

-- | Define your site CSS here
pageStyle :: Css
pageStyle = "div#thesite" ? do
  C.margin (em 4) (pc 5) (em 1) (pc 5)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Source MMark -> SrcMeta
getMeta src = case MMark.projectYaml (Rib.sourceVal src) of
  Nothing -> error "No YAML metadata"
  Just val -> case fromJSON val of
    Aeson.Error (toText -> e) -> error $ "JSON error: " <> e
    Aeson.Success v -> v
