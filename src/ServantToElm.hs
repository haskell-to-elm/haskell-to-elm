{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module ServantToElm where

import Prelude (String)
import Protolude hiding (Type, moduleName)

import qualified Bound
import Control.Lens hiding (Strict)
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

data ElmEncoder = ElmEncoder { _encoder :: Expression Void, _encodedType :: Type Void, _optional :: Bool }
data ElmDecoder = ElmDecoder { _decoder :: Expression Void, _decodedType :: Type Void }

makeEncoder :: forall value a. HasElmEncoder value a => ElmEncoder
makeEncoder = ElmEncoder (elmEncoder @value @a) (elmType @a) False

makeOptionalEncoder :: forall value a. HasElmEncoder value a => ElmEncoder
makeOptionalEncoder = ElmEncoder (elmEncoder @value @a) (elmType @a) True

makeDecoder :: forall value a. HasElmDecoder value a => ElmDecoder
makeDecoder = ElmDecoder (elmDecoder @value @a) (elmType @a)

instance HasElmEncoder Aeson.Value a => HasForeignArgument Elm '[ JSON ] ElmEncoder a where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Aeson.Value @a

instance (SBoolI (FoldRequired mods), HasElmEncoder (RequiredArgument mods Text) b) => HasForeignArgument Elm '[ Header' mods sym a ] ElmEncoder b where
  argumentFor Proxy Proxy Proxy Proxy =
    case sbool @(FoldRequired mods) of
      STrue ->
        makeEncoder @(RequiredArgument mods Text) @b
      SFalse ->
        makeOptionalEncoder @(RequiredArgument mods Text) @b

instance (SBoolI (FoldRequired mods), HasElmEncoder (RequiredArgument mods Text) b) => HasForeignArgument Elm '[ QueryParam' mods sym a ] ElmEncoder b where
  argumentFor Proxy Proxy Proxy Proxy =
    case sbool @(FoldRequired mods) of
      STrue ->
        makeEncoder @(RequiredArgument mods Text) @b
      SFalse ->
        makeOptionalEncoder @(RequiredArgument mods Text) @b

instance HasElmEncoder Text a => HasForeignArgument Elm '[ Capture' mods sym a ] ElmEncoder a where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Text @a

instance HasElmEncoder Text a => HasForeignArgument Elm '[ CaptureAll sym a ] ElmEncoder [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Text @a

instance HasElmEncoder Text a => HasForeignArgument Elm '[ QueryParams sym a ] ElmEncoder [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Text @a

instance HasForeignArgument Elm '[ QueryFlag sym ] ElmEncoder Bool where
  argumentFor Proxy Proxy Proxy Proxy =
    ElmEncoder "Basics.identity" "Basics.Bool" False

instance HasElmDecoder Aeson.Value a => HasForeignResult Elm '[ JSON ] ElmDecoder a where
  resultFor Proxy Proxy Proxy Proxy =
    makeDecoder @Aeson.Value @a

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
  -> Req ElmEncoder ElmDecoder
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
          [ [ _encodedType $ header ^. headerArg . argType
            | header <- req ^. reqHeaders
            ]
          , [ _encodedType $ arg ^. argType . _2
            | Cap arg <- numberedPathSegments
            ]
          ]
        )
        elmReturnType

    elmReturnType =
      let
        type_ =
          maybe "Basics.()" _decodedType (req ^. reqReturnType)
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
          , ("headers", elmHeaders)
          , ( "url", elmUrl)
          , ("body", "TODO.TODO")
          , ("expect", "TODO.TODO")
          , ("timeout", "Maybe.Nothing")
          , ("tracker", "Maybe.Nothing")
          ]
        )

    elmUrl =
      case numberedPathSegments of
        [] ->
          Expression.List []

        [pathSegment] ->
          elmPathSegment pathSegment

        _
          | Just staticPathSegments <- traverse staticPathSegment numberedPathSegments ->
            Expression.String $ Text.intercalate "/" staticPathSegments

        _ ->
          Expression.apps
            "String.join"
            [ Expression.String "/"
            , Expression.List $ elmPathSegment <$> numberedPathSegments
            ]


    elmHeaders =
      let
        headerDecoder i header =
          Expression.apps
            "Http.header"
            [ Expression.String $ unPathSegment $ header ^. headerArg . argName
            , Expression.App
              (vacuous $ _encoder $ header ^. headerArg . argType)
              (pure $ headerArgName i)
            ]

        optionalHeaderDecoder i header =
          Expression.apps
            "Maybe.map"
            [ Expression.App
              "Http.header"
              (Expression.String $ unPathSegment $ header ^. headerArg . argName)
            , Expression.App
              (vacuous $ _encoder $ header ^. headerArg . argType)
              (pure $ headerArgName i)
            ]
      in
      case req ^. reqHeaders of
        [] ->
          Expression.List []

        _
          | any _optional (map (view $ headerArg . argType) $ req ^. reqHeaders) ->
          Expression.apps "List.mapMaybe"
          [ "Basics.identity"
          , Expression.List
              [ if _optional (header ^. headerArg . argType) then
                  optionalHeaderDecoder i header
                else
                  Expression.App "Maybe.Just" $ headerDecoder i header
              | (i, header) <- zip [0..] $ req ^. reqHeaders
              ]
          ]

        _ ->
          Expression.List $
            [ headerDecoder i header
            | (i, header) <- zip [0..] $ req ^. reqHeaders
            ]

    staticPathSegment pathSegment =
      case pathSegment of
        Static (PathSegment s) ->
          Just s

        Cap _ ->
          Nothing

    elmPathSegment pathSegment =
      case pathSegment of
        Static (PathSegment s) ->
          Expression.String s

        Cap arg ->
          Expression.App
            (vacuous $ _encoder $ arg ^. argType . _2)
            (pure $ capturedArgName $ arg ^. argType . _1)

    headerArgName :: Int -> Text
    headerArgName i =
      "header" <> show i

    capturedArgName :: Int -> Text
    capturedArgName i =
      "capture" <> show i

type TestApi
    = "test" :> Header "header" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> Header' '[Required, Strict] "requiredHeader" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "twoheaders" :> Header "optionalHeader" Text :> Header' '[Required, Strict] "requiredHeader" Text :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] NoContent
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] NoContent
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] NoContent
 :<|> "test" :> CaptureAll "ids" Int :> Get '[JSON] [Int]
 :<|> "static" :> "url" :> Get '[JSON] [Int]
 :<|> "test" :> EmptyAPI

testApi :: [Req ElmEncoder ElmDecoder]
testApi =
  listFromAPI (Proxy :: Proxy Elm) (Proxy :: Proxy ElmEncoder) (Proxy :: Proxy ElmDecoder) (Proxy :: Proxy TestApi)

apiTest =
  Pretty.modules $ elmRequest ["MyModule"] <$> testApi
