{-# language DeriveAnyClass #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Elm.Expression where

import Protolude

import Bound
import Data.Bifoldable
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.String
import Text.Show.Deriving

import qualified Language.Elm.Name as Name
import Language.Elm.Pattern (Pattern)
import qualified Language.Elm.Pattern as Pattern

data Expression v
  = Var v
  | Global Name.Qualified
  | App (Expression v) (Expression v)
  | Let (Expression v) (Scope () Expression v)
  | Lam (Scope () Expression v)
  | Record [(Name.Field, Expression v)]
  | Proj Name.Field
  | Case (Expression v) [(Pattern Int, Scope Int Expression v)]
  | If (Expression v) (Expression v) (Expression v)
  | List [Expression v]
  | String !Text
  | Int !Integer
  | Float !Double
  deriving (Functor, Foldable, Traversable)

instance Applicative Expression where
  pure = Var
  (<*>) = ap

instance Monad Expression where
  Var v >>= f = f v
  Global g >>= _ = Global g
  App e1 e2 >>= f = App (e1 >>= f) (e2 >>= f)
  Let e s >>= f = Let (e >>= f) (s >>>= f)
  Lam s >>= f = Lam (s >>>= f)
  Record fs >>= f = Record [(fname, e >>= f) | (fname, e) <- fs]
  Proj f >>= _ = Proj f
  Case e brs >>= f = Case (e >>= f) [(pat, s >>>= f) | (pat, s) <- brs]
  If e e1 e2 >>= f = If (e >>= f) (e1 >>= f) (e2 >>= f)
  List es >>= f = List ((>>= f) <$> es)
  String s >>= _ = String s
  Int n >>= _ = Int n
  Float f >>= _ = Float f

deriving instance Eq v => Eq (Expression v)
deriving instance Ord v => Ord (Expression v)
deriving instance Show v => Show (Expression v)

deriveEq1 ''Expression
deriveOrd1 ''Expression
deriveShow1 ''Expression

instance IsString (Expression v) where
  fromString = Global . fromString

apps :: Foldable f => Expression v -> f (Expression v) -> Expression v
apps = foldl App

appsView :: Expression v -> (Expression v, [Expression v])
appsView = go mempty
  where
    go args expr =
      case expr of
        App e1 e2 ->
          go (e2 : args) e1

        _ ->
          (expr, args)

(|>) :: Expression v -> Expression v -> Expression v
(|>) e1 e2 = apps "Basics.|>" [e1, e2]

(<|) :: Expression v -> Expression v -> Expression v
(<|) e1 e2 = apps "Basics.<|" [e1, e2]

(<<) :: Expression v -> Expression v -> Expression v
(<<) e1 e2 = apps "Basics.<<" [e1, e2]

(>>) :: Expression v -> Expression v -> Expression v
(>>) e1 e2 = apps "Basics.>>" [e1, e2]

(++) :: Expression v -> Expression v -> Expression v
(++) e1 e2 = apps "Basics.++" [e1, e2]

tuple :: Expression v -> Expression v -> Expression v
tuple e1 e2 = apps "Basics.," [e1, e2]

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Expression v
  -> m
foldMapGlobals f expr =
  case expr of
    Var _ ->
      mempty

    Global qname ->
      f qname

    App e1 e2 ->
      foldMapGlobals f e1 <> foldMapGlobals f e2

    Let e s ->
      foldMapGlobals f e <> foldMapGlobals f (Bound.fromScope s)

    Lam s ->
      foldMapGlobals f (Bound.fromScope s)

    Record fields ->
      foldMap (foldMap (foldMapGlobals f)) fields

    Proj _ ->
      mempty

    Case e branches ->
      foldMapGlobals f e <>
      foldMap
        (bifoldMap (Pattern.foldMapGlobals f) (foldMapGlobals f . Bound.fromScope))
        branches

    If e e1 e2 ->
      foldMapGlobals f e <> foldMapGlobals f e1 <> foldMapGlobals f e2

    List es ->
      foldMap (foldMapGlobals f) es

    String _ ->
      mempty

    Int _ ->
      mempty

    Float _ ->
      mempty
