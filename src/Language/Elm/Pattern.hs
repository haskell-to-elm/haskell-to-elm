{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
module Language.Elm.Pattern where

import Protolude

import qualified Language.Elm.Name as Name

data Pattern v
  = Var v
  | Wildcard
  | Con Name.Qualified [Pattern v]
  | List [Pattern v]
  | String !Text
  | Int !Integer
  | Float !Double
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Pattern v
  -> m
foldMapGlobals f pat =
  case pat of
    Var _ ->
      mempty

    Wildcard ->
      mempty

    Con c pats ->
      f c <> foldMap (foldMapGlobals f) pats

    List pats ->
      foldMap (foldMapGlobals f) pats

    String _ ->
      mempty

    Int _ ->
      mempty

    Float _ ->
      mempty

tuple :: Pattern v -> Pattern v -> Pattern v
tuple p1 p2 = Con "Basics.," [p1, p2]
