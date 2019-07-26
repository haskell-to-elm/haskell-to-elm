{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module ServantToElm where

import Prelude (String)
import Protolude hiding (Type)

import qualified Bound
import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Language.Elm.Pretty as Pretty
import Servant.API.Modifiers
import Servant.Foreign

import HaskellToElm
import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pattern as Pattern
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type

data Elm

data ElmEncoder = ElmEncoder (Expression Void) (Type Void) Bool
data ElmDecoder = ElmDecoder (Expression Void) (Type Void) Bool

instance HasElmEncoder Aeson.Value a => HasForeignArgument Elm '[ JSON ] ElmEncoder a where
  argumentFor Proxy Proxy Proxy Proxy =
    (elmEncoder @Aeson.Value @a, elmType @a)

instance HasElmEncoder (RequiredArgument mods Text) b => HasForeignArgument Elm '[ Header' mods sym a ] ElmEncoder b where
  argumentFor Proxy Proxy Proxy Proxy =
    (elmEncoder @(RequiredArgument mods Text) @b, elmType @b)

instance HasElmEncoder (RequiredArgument mods Text) b => HasForeignArgument Elm '[ QueryParam' mods sym a ] (Expression v, Type v') b where
  argumentFor Proxy Proxy Proxy Proxy =
    (elmEncoder @(RequiredArgument mods Text) @b, elmType @b)

instance HasElmEncoder Text a => HasForeignArgument Elm '[ Capture' mods sym a ] ElmEncoder a where
  argumentFor Proxy Proxy Proxy Proxy =
    (elmEncoder @Text @a, elmType @a)

instance HasElmEncoder Text a => HasForeignArgument Elm '[ CaptureAll sym a ] ElmEncoder [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    (elmEncoder @Text @a, elmType @a)

instance HasElmEncoder Text a => HasForeignArgument Elm '[ QueryParams sym a ] ElmEncoder [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    (elmEncoder @Text @a, elmType @a)

instance HasForeignArgument Elm '[ QueryFlag sym ] ElmEncoder Bool where
  argumentFor Proxy Proxy Proxy Proxy =
    ("Basics.identity", "Basics.Bool")

instance HasElmDecoder Aeson.Value a => HasForeignResult Elm '[ JSON ] ElmDecoder a where
  resultFor Proxy Proxy Proxy Proxy =
    (elmDecoder @Aeson.Value @a, elmType @a)

instance HasElmType NoContent where
  elmType =
    "NoContent.NoContent"

instance HasElmDecoder Aeson.Value NoContent where
  elmDecoder =
    "Json.Decode.string" Expression.|>
      Expression.App "Json.Decode.andThen"
      (Expression.Lam $ Bound.toScope $
        Expression.Case
          (Expression.Var $ Bound.B ())
          [ ( Pattern.String ""
            , Bound.toScope $ Expression.App "Json.Decode.succeed" "NoContent.NoContent"
            )
          , ( Pattern.Wildcard
            , Bound.toScope $ Expression.App "Json.Decode.fail" $ Expression.String "Expected no content"
            )
          ]
      )

elmRequest
  :: Name.Module
  -> Req ElmEncoder ElmEncoder
  -> Definition
elmRequest moduleName req =
  Definition.Constant
    (Name.Qualified moduleName elmFunctionName)
    elmType
    (panic "expression not closed" <$> lambdaArgs argNames elmBody)
  where
    elmFunctionName :: Text
    elmFunctionName =
      case req ^. reqFuncName of
        FunctionName [] ->
          panic "empty function name"

        FunctionName (part:parts) ->
          mconcat (part : map capitalise parts)

      where
        capitalise :: Text -> Text
        capitalise part =
          case Text.uncons part of
            Just (p, art) ->
              Text.toUpper (Text.singleton p) <> art

            Nothing ->
              ""

    elmType :: Type Void
    elmType =
      Type.funs
        (concat
          [ [ header ^. headerArg . argType . _2
            | header <- req ^. reqHeaders
            ]
          , [ arg ^. argType . _2 . _2
            | Cap arg <- numberedPathSegments
            ]
          ]
        )
        elmReturnType

    elmReturnType =
      let
        type_ =
          maybe "Basics.()" snd (req ^. reqReturnType)
      in
      Type.App "Platform.Cmd.Cmd" (Type.apps "Result.Result" ["Http.Error", type_])

    numberedPathSegments =
      go 0 $ req ^. reqUrl . path
      where
        go !i segments =
          case segments of
            [] ->
              []

            Segment (Static p):segments' ->
              Static p : go i segments'

            Segment (Cap arg):segments' ->
              Cap ((,) i <$> arg) : go (i + 1) segments'

    capturedArgs =
      [arg | Cap arg <- numberedPathSegments]

    argNames =
      concat
      [ [ headerArgName i
        | (i, _) <- zip [0..] $ req ^. reqHeaders
        ]
      , [ capturedArgName $ arg ^. argType . _1
        | Cap arg <- numberedPathSegments
        ]
      ]

    lambdaArgs :: [Text] -> Expression Text -> Expression Text
    lambdaArgs args rhs =
      case args of
        [] ->
          rhs

        arg:args' ->
          Expression.Lam $ Bound.abstract1 arg $ lambdaArgs args' rhs

    elmBody :: Expression Text
    elmBody =
      Expression.App
        "Http.request"
        (Expression.Record
          [ ("method", Expression.String $ toS $ req ^. reqMethod)
          , ("headers"
            , Expression.List
              [ Expression.tuple (Expression.String name) (Expression.App (vacuous $ arg ^. argType . _1) (pure $ headerArgName i))
              | (i, header) <- zip [0..] $ req ^. reqHeaders
              , let
                  arg =
                    header ^. headerArg

                  name =
                    unPathSegment (arg ^. argName)
              ]
            )
          , ( "url"
            , Expression.apps
                "String.join"
                [ Expression.String "/"
                , Expression.List $ elmPathSegment <$> numberedPathSegments
                ]
            )
          , ("body", "TODO.TODO")
          , ("expect", "TODO.TODO")
          , ("timeout", "Maybe.Nothing")
          , ("tracker", "Maybe.Nothing")
          ]
        )

    elmPathSegment pathSegment =
      case pathSegment of
        Static (PathSegment s) ->
          Expression.String s

        Cap arg ->
          Expression.App
            (vacuous $ arg ^. argType . _2 . _1)
            (pure $ capturedArgName $ arg ^. argType . _1)

    headerArgName :: Int -> Text
    headerArgName i =
      "header" <> show i

    capturedArgName :: Int -> Text
    capturedArgName i =
      "capture" <> show i

type TestApi
    = "test" :> Header "header" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] NoContent
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] NoContent
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] NoContent
 :<|> "test" :> CaptureAll "ids" Int :> Get '[JSON] [Int]
 :<|> "test" :> EmptyAPI

testApi :: [Req (Expression v, Type v') (Expression v, Type v')]
testApi =
  listFromAPI (Proxy :: Proxy Elm) (Proxy :: Proxy (Expression v, Type v')) (Proxy :: Proxy (Expression v, Type v')) (Proxy :: Proxy TestApi)

apiTest =
  Pretty.modules $ elmRequest ["MyModule"] <$> testApi
