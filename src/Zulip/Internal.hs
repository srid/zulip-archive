{-# LANGUAGE TemplateHaskell #-}

module Zulip.Internal where

import Data.Aeson
import Data.Char (isUpper)
import Text.Casing (fromHumps, toQuietSnake)

fieldLabelMod :: Options
fieldLabelMod =
  defaultOptions
    { fieldLabelModifier = toQuietSnake . fromHumps . dropWhile (not . isUpper)
    }
