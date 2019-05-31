{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
module Language.Elm.Pretty where

import Protolude hiding (local, list)

import qualified Bound
import qualified Bound.Var as Bound
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.Text.Prettyprint.Doc

import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import Language.Elm.Pattern (Pattern)
import qualified Language.Elm.Pattern as Pattern

data Environment v = Environment
  { locals :: v -> Name.Local
  , freshLocals :: [Name.Local]
  }

extend :: Environment v -> (Environment (Bound.Var () v), Name.Local)
extend env =
  case freshLocals env of
    [] ->
      panic "Language.Elm.Pretty no locals"

    fresh:freshLocals' ->
      ( env
        { locals = Bound.unvar (\() -> fresh) (locals env)
        , freshLocals = freshLocals'
        }
      , fresh
      )

extendPat :: Environment v -> Pattern Int -> Environment (Bound.Var Int v)
extendPat env pat =
  let
    occurrencesSet =
      foldMap HashSet.singleton pat

    occurrences =
      HashSet.toList occurrencesSet

    bindings =
      HashMap.fromList $
        zip occurrences $ freshLocals env

    freshLocals' =
      drop (length occurrences) $ freshLocals env

    lookupVar i =
      case HashMap.lookup i bindings of
        Nothing ->
          panic "Unbound pattern variable"

        Just v ->
          v
  in
  env
    { locals = Bound.unvar lookupVar (locals env)
    , freshLocals = freshLocals'
    }


local :: Name.Local -> Doc ann
local (Name.Local l) =
  pretty l

field :: Name.Field -> Doc ann
field (Name.Field f) =
  pretty f

defaultImport :: Name.Qualified -> Maybe Name.Local
defaultImport qname =
  case qname of
    -- Name.Qualified ["Basics"] name ->
    --   Just $ Name.Local name

    "List.List" ->
      Just "List"

    "List.::" ->
      Just "::"

    "Maybe.Maybe" ->
      Just "Maybe"

    "Maybe.Nothing" ->
      Just "Nothing"

    "Maybe.Just" ->
      Just "Just"

    "Result.Result" ->
      Just "Result"

    "Result.Ok" ->
      Just "Ok"

    "Result.Err" ->
      Just "Err"

    "String.String" ->
      Just "String"

    "Char.Char" ->
      Just "Char"

    _ -> Nothing

qualified :: Name.Qualified -> Doc ann
qualified name@(Name.Qualified modules l) =
  case defaultImport name of
    Nothing ->
      case modules of
        [] ->
          pretty l

        _ ->
          mconcat (intersperse dot $ pretty <$> modules) <> dot <> pretty l
    Just l' ->
      local l'

expression :: Environment v -> Int -> Expression v -> Doc ann
expression env prec expr =
  case expr of
    Expression.Var var ->
      local $ locals env var

    Expression.Global name ->
      qualified name

    Expression.App expr1 expr2 ->
      parensWhen (prec > appPrec) $
        expression env appPrec expr1 <+> expression env (appPrec + 1) expr2

    Expression.Let {} ->
      parensWhen (prec > letPrec) $
        let
          (bindings, body) =
            lets env expr
        in
        "let"
        <> line <> indent 4 (mconcat $ intersperse (line <> line) bindings)
        <> line <> "in"
        <> line <> body

    Expression.Lam {} ->
      parensWhen (prec > lamPrec) $
        let
          (names, body) =
            lambdas env expr
        in
        "\\" <> hsep (local <$> names) <+> "->" <+> body

    Expression.Record fields ->
      encloseSep "{ " " }" ", "
        [ field f <+> "=" <+> expression env 0 expr'
        | (f, expr') <- fields
        ]

    Expression.Proj f ->
      "." <> field f

    Expression.Case expr' branches ->
      parensWhen (prec > casePrec) $
        "case" <+> expression env 0 expr' <+> "of" <> line <>
        indent 4
        (
        mconcat $
        intersperse (line <> line) $
          [ pattern env' 0 pat <+> "->" <+> expression env' 0 (Bound.fromScope scope)
          | (pat, scope) <- branches
          , let
              env' =
                extendPat env pat
          ]
        )

    Expression.List exprs ->
      list $ expression env 0 <$> exprs

    Expression.String s ->
      "\"" <> pretty s <> "\""

    Expression.Int i ->
      pretty i

    Expression.Float f ->
      pretty f

lets :: Environment v -> Expression v -> ([Doc ann], Doc ann)
lets env expr =
  case expr of
    Expression.Let expr' scope ->
      let
        (env', name) =
          extend env

        (bindings, body) =
          lets env' (Bound.fromScope scope)

        binding =
          local name <+> "="
            <> line <> indent 4 (expression env 0 expr')

      in
      (binding : bindings , body)

    _ ->
      ([], expression env letPrec expr)

lambdas :: Environment v -> Expression v -> ([Name.Local], Doc ann)
lambdas env expr =
  case expr of
    Expression.Lam scope ->
      let
        (env', name) =
          extend env

        (names, body) =
          lambdas env' (Bound.fromScope scope)
      in
      (name : names, body)

    _ ->
      ([], expression env lamPrec expr)

pattern :: Environment (Bound.Var Int v) -> Int -> Pattern Int -> Doc ann
pattern env prec pat =
  case pat of
    Pattern.Var var ->
      local $ locals env (Bound.B var)

    Pattern.Wildcard ->
      "_"

    Pattern.Con con [] ->
      qualified con

    Pattern.Con con pats ->
      parensWhen (prec > appPrec) $
        qualified con <+> hsep (pattern env (appPrec + 1) <$> pats)

    Pattern.String s ->
      "\"" <> pretty s <> "\""

    Pattern.Int i ->
      pretty i

    Pattern.Float f ->
      pretty f

parensWhen :: Bool -> Doc ann -> Doc ann
parensWhen b =
  if b then
    parens

  else
    identity

appPrec, letPrec, lamPrec, casePrec :: Int
appPrec = 10
letPrec = 0
lamPrec = 0
casePrec = 0
