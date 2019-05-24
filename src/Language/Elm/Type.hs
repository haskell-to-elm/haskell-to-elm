{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
module Language.Elm.Type where

import Protolude hiding (Type)

import Data.String

import qualified Language.Elm.Name as Name

data Type v
  = Var v
  | Global Name.Qualified
  | App (Type v) (Type v)
  | Record [(Name.Field, Type v)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Type where
  pure = Var
  (<*>) = ap

instance Monad Type where
  Var v >>= f = f v
  Global g >>= _ = Global g
  App t1 t2 >>= f = App (t1 >>= f) (t2 >>= f)
  Record fields >>= f = Record [(n, t >>= f) | (n, t) <- fields]

instance IsString (Type v) where
  fromString = Global . fromString

appsView :: Type v -> (Type v, [Type v])
appsView = go mempty
  where
    go args typ =
      case typ of
        App t1 t2 ->
          go (t2 : args) t1

        _ ->
          (typ, args)
