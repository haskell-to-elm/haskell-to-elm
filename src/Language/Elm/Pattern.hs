{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
module Language.Elm.Pattern where

import Protolude

import qualified Language.Elm.Name as Name

data Pattern v
  = Var v
  | Wildcard
  | Con Name.Qualified [Pattern v]
  | String !Text
  | Number !Double
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
