{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
module Language.Elm.Type where

import Protolude hiding (Type)

import Data.String

import qualified Language.Elm.Name as Name

data Type v
  = Var v
  | App (Type v) (Type v)
  | Record [(Name.Field, Type v)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Type where
  pure = Var
  (<*>) = ap

instance Monad Type where
  Var v >>= f = f v
  App t1 t2 >>= f = App (t1 >>= f) (t2 >>= f)
  Record fields >>= f = Record [(n, t >>= f) | (n, t) <- fields]

instance IsString v => IsString (Type v) where
  fromString = Var . fromString
