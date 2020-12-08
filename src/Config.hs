{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config where

import Dhall (auto, input)
import Dhall.TH (makeHaskellTypeFromUnion)
import Relude
import System.Directory (withCurrentDirectory)
import System.Environment (lookupEnv)

makeHaskellTypeFromUnion "Config" "< Config : ./config/Type.dhall >"

readConfig :: MonadIO m => m Config
readConfig = do
  expr <-
    fromMaybe "./config.dhall"
      <$> liftIO (lookupEnv "ZULIP_ARCHIVE_CONFIG")
  liftIO $
    withCurrentDirectory "config" $
      input auto (toText expr)
