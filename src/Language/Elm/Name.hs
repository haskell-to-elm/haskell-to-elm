{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Elm.Name where

import Protolude

import Data.String
import qualified Data.Text as Text

type Module = [Text]

newtype Local = Local Text
  deriving (Eq, Ord, Show, IsString, Hashable)

data Qualified = Qualified Module Text
  deriving (Eq, Ord, Show)

instance IsString Qualified where
  fromString s =
    case unsnoc $ Text.splitOn "." $ fromString s of
      Nothing ->
        panic "Empty name"

      Just (xs, x) ->
        Qualified xs x

newtype Field = Field Text
  deriving (Eq, Ord, Show, IsString, Hashable)

newtype Constructor = Constructor Text
  deriving (Eq, Ord, Show, IsString)
