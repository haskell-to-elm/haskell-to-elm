module Language.Elm.Definition where

import qualified Language.Elm.Name as Name
import Language.Elm.Expression
import Language.Elm.Type

data Definition
  = Constant !Name.Qualified (Type Name.Qualified) (Expression Name.Qualified)
  | Type !Name.Qualified [(Name.Constructor, [Type Name.Qualified])]
  | Alias !Name.Qualified (Type Name.Qualified)
  deriving (Eq, Ord, Show)

name :: Definition -> Name.Qualified
name (Constant n _ _) = n
name (Type n _) = n
name (Alias n _) = n
