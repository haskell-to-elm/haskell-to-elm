{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
module Language.Elm.Type where

import Protolude hiding (Type)

import Data.String

import qualified Language.Elm.Name as Name

data Type v
  = Var v
  | Global Name.Qualified
  | App (Type v) (Type v)
  | Fun (Type v) (Type v)
  | Record [(Name.Field, Type v)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Type where
  pure = Var
  (<*>) = ap

instance Monad Type where
  Var v >>= f = f v
  Global g >>= _ = Global g
  App t1 t2 >>= f = App (t1 >>= f) (t2 >>= f)
  Fun t1 t2 >>= f = Fun (t1 >>= f) (t2 >>= f)
  Record fields >>= f = Record [(n, t >>= f) | (n, t) <- fields]

instance IsString (Type v) where
  fromString = Global . fromString

apps :: Type v -> [Type v] -> Type v
apps = foldl' App

appsView :: Type v -> (Type v, [Type v])
appsView = go mempty
  where
    go args typ =
      case typ of
        App t1 t2 ->
          go (t2 : args) t1

        _ ->
          (typ, args)

funs :: [Type v] -> Type v -> Type v
funs args ret =
  foldr Fun ret args

tuple :: Type v -> Type v -> Type v
tuple t1 t2 = apps "Basics.," [t1, t2]

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Type v
  -> m
foldMapGlobals f type_ =
  case type_ of
    Var _ ->
      mempty

    Global qname ->
      f qname

    App t1 t2 ->
      foldMapGlobals f t1 <> foldMapGlobals f t2

    Fun t1 t2 ->
      foldMapGlobals f t1 <> foldMapGlobals f t2

    Record fields ->
      foldMap (foldMap (foldMapGlobals f)) fields
