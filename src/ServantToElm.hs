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

import Protolude hiding (Type, moduleName)

import qualified Bound
import Control.Lens hiding (Strict, List)
import qualified Data.Aeson as Aeson
import qualified Language.Elm.Pretty as Pretty
import Servant.API ((:<|>), (:>))
import qualified Servant.API.Modifiers as Servant
import qualified Servant.Foreign as Servant

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

instance HasElmEncoder Aeson.Value a => Servant.HasForeignArgument Elm '[ Servant.JSON ] ElmEncoder a where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Aeson.Value @a

instance (Servant.SBoolI (Servant.FoldRequired mods), HasElmEncoder (Servant.RequiredArgument mods Text) b) => Servant.HasForeignArgument Elm '[ Servant.Header' mods sym a ] ElmEncoder b where
  argumentFor Proxy Proxy Proxy Proxy =
    case Servant.sbool @(Servant.FoldRequired mods) of
      Servant.STrue ->
        makeEncoder @(Servant.RequiredArgument mods Text) @b
      Servant.SFalse ->
        makeOptionalEncoder @(Servant.RequiredArgument mods Text) @b

instance (Servant.SBoolI (Servant.FoldRequired mods), HasElmEncoder (Servant.RequiredArgument mods Text) b) => Servant.HasForeignArgument Elm '[ Servant.QueryParam' mods sym a ] ElmEncoder b where
  argumentFor Proxy Proxy Proxy Proxy =
    case Servant.sbool @(Servant.FoldRequired mods) of
      Servant.STrue ->
        makeEncoder @(Servant.RequiredArgument mods Text) @b

      Servant.SFalse ->
        makeOptionalEncoder @(Servant.RequiredArgument mods Text) @b

instance HasElmEncoder Text a => Servant.HasForeignArgument Elm '[ Servant.Capture' mods sym a ] ElmEncoder a where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Text @a

instance HasElmEncoder Text a => Servant.HasForeignArgument Elm '[ Servant.CaptureAll sym a ] ElmEncoder [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Text @a

instance HasElmEncoder Text a => Servant.HasForeignArgument Elm '[ Servant.QueryParams sym a ] ElmEncoder [a] where
  argumentFor Proxy Proxy Proxy Proxy =
    makeEncoder @Text @a

instance Servant.HasForeignArgument Elm '[ Servant.QueryFlag sym ] ElmEncoder Bool where
  argumentFor Proxy Proxy Proxy Proxy =
    ElmEncoder "Basics.identity" "Basics.Bool" False

instance HasElmDecoder Aeson.Value a => Servant.HasForeignResult Elm '[ Servant.JSON ] ElmDecoder a where
  resultFor Proxy Proxy Proxy Proxy =
    makeDecoder @Aeson.Value @a

instance HasElmType Servant.NoContent where
  elmType =
    "NoContent.NoContent"

instance HasElmDecoder Aeson.Value Servant.NoContent where
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
  :: Expression Void
  -> Name.Module
  -> Servant.Request ElmEncoder ElmDecoder
  -> Definition
elmRequest urlBase moduleName req =
  Definition.Constant
    (Name.Qualified moduleName $ Servant.camelCase $ req ^. Servant.functionName)
    elmTypeSig
    (panic "expression not closed" <$> lambdaArgs argNames elmLambdaBody)
  where
    elmTypeSig :: Type Void
    elmTypeSig =
      Type.funs
        (concat
          [ [ _encodedType arg
            | (_, arg) <- req ^. Servant.headers
            ]
          , [ _encodedType arg
            | Servant.Capture _ (_, arg) <- numberedPathSegments
            ]
          , [ case type_ of
                Servant.Normal ->
                  vacuous $ _encodedType arg

                Servant.Flag ->
                  vacuous $ _encodedType arg

                Servant.List ->
                  vacuous $ Type.App "List.List" $ _encodedType arg
            | (_, type_, arg) <- req ^. Servant.url . Servant.queryString
            ]
          , [ _encodedType body
            | Just body <- [req ^. Servant.body]
            ]
          ]
        )
        elmReturnType

    elmReturnType =
      let
        type_ =
          maybe "Basics.()" _decodedType (req ^. Servant.returnType)
      in
      Type.App
        "Cmd.Cmd"
        (Type.apps
          "Result.Result"
          [Type.tuple "Http.Error" (Type.App "Maybe.Maybe" $ Type.tuple "Http.Metadata" "String.String"), type_]
        )

    elmReturnDecoder =
      case req ^. Servant.returnType of
        Nothing ->
          panic "elmRequest: No return type" -- TODO?

        Just ret ->
          vacuous $ _decoder ret

    numberedPathSegments =
      go 0 $ req ^. Servant.url . Servant.path
      where
        go !i segments =
          case segments of
            [] ->
              []

            Servant.Static p:segments' ->
              Servant.Static p : go i segments'

            Servant.Capture str arg:segments' ->
              Servant.Capture str (i, arg) : go (i + 1) segments'

    argNames =
      concat
      [ [ headerArgName i
        | (i, _) <- zip [0..] $ req ^. Servant.headers
        ]
      , [ capturedArgName i
        | Servant.Capture _ (i, _) <- numberedPathSegments
        ]
      , [ paramArgName i
        | (i, _) <- zip [0..] $ req ^. Servant.url . Servant.queryString
        ]
      , [ bodyArgName
        | Just _ <- [req ^. Servant.body]
        ]
      ]

    lambdaArgs :: [Text] -> Expression Text -> Expression Text
    lambdaArgs args rhs =
      case args of
        [] ->
          rhs

        arg:args' ->
          Expression.Lam $ Bound.abstract1 arg $ lambdaArgs args' rhs

    elmLambdaBody :: Expression Text
    elmLambdaBody =
      Expression.App
        "Http.request"
        (Expression.Record
          [ ("method", Expression.String $ toS $ req ^. Servant.method)
          , ("headers", elmHeaders)
          , ("url", elmUrl)
          , ("body", elmBody)
          , ("expect", elmExpect)
          , ("timeout", "Maybe.Nothing")
          , ("tracker", "Maybe.Nothing")
          ]
        )

    elmParams =
      [ case type_ of
        Servant.Normal ->
          if _optional arg then
            Expression.apps
              "Maybe.unwrap"
              [ Expression.List []
              , "List.singleton" Expression.<< Expression.App "Basics.++" (Expression.String $ name <> "=")
              , encode $ pure $ paramArgName i
              ]

          else
            Expression.List
              [Expression.String (name <> "=") Expression.++ encode (pure $ paramArgName i)]

        Servant.Flag ->
          Expression.If
            (pure $ paramArgName i)
            (Expression.List [Expression.String name])
            (Expression.List [])

        Servant.List ->
          Expression.apps
            "List.map"
            [ Expression.App "Basics.++" (Expression.String (name <> "[]=")) Expression.<< encoder
            , pure $ paramArgName i
            ]
      | (i, (name, type_, arg)) <- zip [0..] $ req ^. Servant.url . Servant.queryString
      , let
          encoder =
            vacuous $ _encoder arg

          encode =
            Expression.App encoder
      ]

    elmUrl =
      case elmParams of
        [] ->
          withoutParams

        [elmParams'] ->
          withParams elmParams'

        _ ->
          withParams (Expression.App "List.concat" $ Expression.List elmParams)

      where
        withoutParams =
          Expression.apps
            "String.join"
            [ Expression.String "/"
            , Expression.List $ vacuous urlBase : fmap elmPathSegment numberedPathSegments
            ]

        withParams params =
          withoutParams Expression.++
            Expression.Case params
              [ (Pattern.List [], Bound.toScope $ Expression.String "")
              , ( Pattern.Var 0
                , Bound.toScope $ Expression.String "?" Expression.++ Expression.apps "String.join" [Expression.String "&", pure $ Bound.B 0]
                )
              ]


    elmHeaders =
      let
        headerDecoder i name arg  =
          Expression.apps
            "Http.header"
            [ Expression.String name
            , Expression.App
              (vacuous $ _encoder arg)
              (pure $ headerArgName i)
            ]

        optionalHeaderDecoder i name arg =
          Expression.apps
            "Maybe.map"
            [ Expression.App
              "Http.header"
              (Expression.String name)
            , Expression.App
              (vacuous $ _encoder arg)
              (pure $ headerArgName i)
            ]
      in
      case req ^. Servant.headers of
        [] ->
          Expression.List []

        _
          | any (_optional . snd) (req ^. Servant.headers) ->
          Expression.apps "List.mapMaybe"
          [ "Basics.identity"
          , Expression.List
              [ if _optional arg then
                  optionalHeaderDecoder i name arg
                else
                  Expression.App "Maybe.Just" $ headerDecoder i name arg
              | (i, (name, arg)) <- zip [0..] $ req ^. Servant.headers
              ]
          ]

        _ ->
          Expression.List
            [ headerDecoder i name arg
            | (i, (name, arg)) <- zip [0..] $ req ^. Servant.headers
            ]

    elmBody =
      case req ^. Servant.body of
        Nothing ->
          "Http.emptyBody"

        Just body ->
          Expression.App
            "Http.jsonBody"
            (Expression.App (vacuous $ _encoder body) $ pure bodyArgName)

    elmExpect =
      Expression.apps
        "Http.expectStringResponse"
        [ "Basics.identity"
        , Expression.Lam $ Bound.toScope $
            Expression.Case (pure $ Bound.B ())
            [ ( Pattern.Con "Http.BadUrl_" [Pattern.Var 0]
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple (Expression.App "Http.BadUrl" $ pure (Bound.B 0)) "Maybe.Nothing"
              )
            , ( Pattern.Con "Http.Timeout_" []
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple "Http.Timeout" "Maybe.Nothing"
              )
            , ( Pattern.Con "Http.NetworkError_" []
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple "Http.NetworkError" "Maybe.Nothing"
              )
            , ( Pattern.Con "Http.BadStatus" [Pattern.Var 0, Pattern.Var 1]
              , Bound.toScope $
                Expression.App "Result.Err" $
                Expression.tuple
                  (Expression.App "Http.BadStatus" (Expression.App (Expression.Proj "statusCode") $ pure $ Bound.B 0))
                  (Expression.App "Maybe.Just" $ Expression.tuple (pure $ Bound.B 0) (pure $ Bound.B 1))
              )
            , ( Pattern.Con "Http.GoodStatus" [Pattern.Var 0, Pattern.Var 1]
              , Bound.toScope $
                Expression.apps "Result.mapError"
                  [ Expression.Lam $ Bound.toScope $
                    Expression.tuple
                      (Expression.App "Http.BadBody" $
                        Expression.App "Json.Decode.errorToString" $
                        pure $ Bound.B ()
                      )
                      (Expression.App "Maybe.Just" $ Expression.tuple (pure $ Bound.F $ Bound.B 0) (pure $ Bound.F $ Bound.B 1))
                  , Expression.apps "Json.Decode.decodeString" [elmReturnDecoder, pure $ Bound.B 1]
                  ]
              )
            ]
        ]

    elmPathSegment pathSegment =
      case pathSegment of
        Servant.Static s ->
          Expression.String s

        Servant.Capture _ (i, arg) ->
          Expression.App
            (vacuous $ _encoder arg)
            (pure $ capturedArgName i)

    bodyArgName :: Text
    bodyArgName =
      "body"

    headerArgName :: Int -> Text
    headerArgName i =
      "header" <> show i

    capturedArgName :: Int -> Text
    capturedArgName i =
      "capture" <> show i

    paramArgName :: Int -> Text
    paramArgName i =
      "param" <> show i

type TestApi
    = "header" :> Servant.Header "header" Text :> Servant.QueryFlag "flag" :> Servant.Get '[Servant.JSON] Int
 :<|> "strictheader" :> Servant.Header' '[Servant.Required, Servant.Strict] "requiredHeader" Text :> Servant.QueryFlag "flag" :> Servant.Get '[Servant.JSON] Int
 :<|> "twoheaders" :> Servant.Header "optionalHeader" Text :> Servant.Header' '[Servant.Required, Servant.Strict] "requiredHeader" Text :> Servant.QueryFlag "flag" :> Servant.Get '[Servant.JSON] Int
 :<|> "paramandbody" :> Servant.QueryParam "param" Int :> Servant.ReqBody '[Servant.JSON] [Text] :> Servant.Post '[Servant.JSON] Servant.NoContent
 :<|> "requiredparamandbody" :> Servant.QueryParam' '[Servant.Required, Servant.Strict] "param" Int :> Servant.ReqBody '[Servant.JSON] [Text] :> Servant.Post '[Servant.JSON] Servant.NoContent
 :<|> "paramsandbody" :> Servant.QueryParams "params" Int :> Servant.ReqBody '[Servant.JSON] Text :> Servant.Put '[Servant.JSON] Servant.NoContent
 :<|> "capture" :> Servant.Capture "id" Int :> Servant.Delete '[Servant.JSON] Servant.NoContent
 :<|> "captures" :> Servant.CaptureAll "ids" Int :> Servant.Get '[Servant.JSON] [Int]
 :<|> "static" :> "url" :> Servant.Get '[Servant.JSON] [Int]

testApi :: [Servant.Request ElmEncoder ElmDecoder]
testApi =
  Servant.listFromAPI (Proxy :: Proxy Elm) (Proxy :: Proxy ElmEncoder) (Proxy :: Proxy ElmDecoder) (Proxy :: Proxy TestApi)

apiTest =
  Pretty.modules $ elmRequest "Config.urlBase" ["My", "Module"] <$> testApi

-- TODO: Empty responses
