{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Taken from Web.Slug in hackage (no longer maintained)
module Slug where

import Relude
import Control.Monad.Catch (MonadThrow (..))
import Data.Char (isAlphaNum)
import qualified Data.Text as T

newtype Slug = Slug Text
  deriving (Eq, Ord, Typeable)

instance Semigroup Slug where
  x <> y = Slug (unSlug x <> "-" <> unSlug y)

data SlugException
  = -- | Slug cannot be generated for given text
    InvalidInput Text
  | -- | Input is not a valid slug, see 'parseSlug'
    InvalidSlug Text
  | -- | Requested slug length is not a positive number
    InvalidLength Int
  deriving (Eq, Show, Typeable)

instance Exception SlugException where
  displayException (InvalidInput text) = "Cannot build slug for " ++ show text
  displayException (InvalidSlug text) = "The text is not a valid slug " ++ show text
  displayException (InvalidLength n) = "Invalid slug length: " ++ show n

unSlug :: Slug -> Text
unSlug (Slug x) = x

mkSlug :: MonadThrow m => Text -> m Slug
mkSlug text = do
  let ws = getSlugWords text
  if null ws
    then throwM (InvalidInput text)
    else return . Slug . T.intercalate "-" $ ws

getSlugWords :: Text -> [Text]
getSlugWords = T.words . T.toLower . T.map f . T.replace "'" ""
  where
    f x = if isAlphaNum x then x else ' '
