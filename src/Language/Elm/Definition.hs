module Language.Elm.Definition where

import Protolude hiding (Type)

import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type

data Definition
  = Constant !Name.Qualified (Type Void) (Expression Void)
  | Type !Name.Qualified [(Name.Constructor, [Type Void])]
  | Alias !Name.Qualified (Type Void)
  deriving (Eq, Ord, Show)

name :: Definition -> Name.Qualified
name (Constant n _ _) = n
name (Type n _) = n
name (Alias n _) = n

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Definition
  -> m
foldMapGlobals f def =
  case def of
    Constant qname type_ expr ->
      f qname <>
      Type.foldMapGlobals f type_ <>
      Expression.foldMapGlobals f expr

    Type qname constrs ->
      f qname <>
      foldMap (foldMap (foldMap (Type.foldMapGlobals f))) constrs

    Alias qname type_ ->
      f qname <>
      Type.foldMapGlobals f type_
