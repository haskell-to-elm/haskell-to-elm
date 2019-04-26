{-# language DeriveAnyClass #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
module Language.Elm.Expression where

import Protolude

import Bound
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.String
import Text.Show.Deriving

import qualified Language.Elm.Name as Name

data Expression v
  = Var v
  | App (Expression v) (Expression v)
  | Let (Expression v) (Scope () Expression v)
  | Lam (Scope () Expression v)
  | Proj Name.Field
  deriving (Functor, Foldable, Traversable)

instance Applicative Expression where
  pure = Var
  (<*>) = ap

instance Monad Expression where
  Var v >>= f = f v
  App e1 e2 >>= f = App (e1 >>= f) (e2 >>= f)
  Let e s >>= f = Let (e >>= f) (s >>>= f)
  Lam s >>= f = Lam (s >>>= f)
  Proj f >>= _ = Proj f

deriving instance Eq v => Eq (Expression v)
deriving instance Ord v => Ord (Expression v)
deriving instance Show v => Show (Expression v)

deriveEq1 ''Expression
deriveOrd1 ''Expression
deriveShow1 ''Expression

instance IsString v => IsString (Expression v) where
  fromString = Var . fromString

apps :: Foldable f => Expression v -> f (Expression v) -> Expression v
apps = foldl App
