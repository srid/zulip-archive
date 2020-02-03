{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Config where

import Dhall
import Dhall.TH
import Relude
import System.Directory (withCurrentDirectory)

makeHaskellTypeFromUnion "Config" "< Config : ./config/Type.dhall >"

deriving instance Generic Config

deriving instance FromDhall Config

readConfig :: MonadIO m => m Config
readConfig =
  liftIO $ withCurrentDirectory "config" $
    input auto "./config.dhall"
