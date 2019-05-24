module Language.Elm.Definition where

import Protolude hiding (Type)

import qualified Language.Elm.Name as Name
import Language.Elm.Expression
import Language.Elm.Type

data Definition
  = Constant !Name.Qualified (Type Void) (Expression Void)
  | Type !Name.Qualified [(Name.Constructor, [Type Void])]
  | Alias !Name.Qualified (Type Void)
  deriving (Eq, Ord, Show)

name :: Definition -> Name.Qualified
name (Constant n _ _) = n
name (Type n _) = n
name (Alias n _) = n
