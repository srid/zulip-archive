{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zulip.Internal where

import Control.Monad
import Data.Aeson
import Data.Char (isUpper)
import Text.Casing (fromHumps, toQuietSnake)
import qualified Text.URI as URI

fieldLabelMod :: Options
fieldLabelMod =
  defaultOptions
    { fieldLabelModifier = toQuietSnake . fromHumps . dropWhile (not . isUpper)
    }

instance FromJSON URI.URI where
    parseJSON = parseJSON >=> either (fail . show) return . URI.mkURI

instance ToJSON URI.URI where
    toJSON = toJSON . URI.render
