{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.UniqSlug where

import Crypto.Hash (Digest, MD5 (..), hash)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Relude hiding (Option)
import Web.Slug

-- | Make a non-injective function injective
mkInjective :: Ord b => [a] -> (a -> b) -> a -> (b, Maybe a)
mkInjective domain f a =
  if Set.member b nonInjectiveImage
    then (b, Just a)
    else (b, Nothing)
  where
    image = map f domain
    nonInjectiveImage = Set.fromList $ dups image
    b = f a
    dups = Map.keys . Map.filter (> 1) . Map.fromListWith (+) . fmap (,1 :: Int)

mkInjectiveWith :: Ord b => (a -> b -> b) -> [a] -> (a -> b) -> a -> b
mkInjectiveWith g domain f =
  mkInjective domain f >>> \case
    (b, Nothing) -> b
    (b, Just a) -> g a b

mkInjectiveWithHash :: forall a b. (Ord b, Monoid b, IsString b, ConvertUtf8 a ByteString) => [a] -> (a -> b) -> a -> b
mkInjectiveWithHash =
  mkInjectiveWith $ \a b -> b <> "-" <> textHash a
  where
    textHash s =
      let digest :: Digest MD5
          digest = hash $ encodeUtf8 @a @ByteString s
       in show digest

-- | Make a slug of second argument, ensuring that it is unique across slugs generated from the first argument.
-- If a duplicate is found, append a md5 hash to make it unique.
mkUniqSlug :: [Text] -> Text -> Text
mkUniqSlug xs =
  mkInjectiveWithHash xs mkSlugPure
  where
    mkSlugPure =
      either (error . toText . displayException) unSlug . mkSlug
