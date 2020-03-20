{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Parameterised where

import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import qualified Generics.SOP as SOP
import GHC.Generics

import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Type as Type
import Language.Haskell.To.Elm

data Result e a
  = Err e
  | Ok a
  deriving (Generic, Aeson.ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType Result where
  elmDefinition =
    Just $ deriveElmTypeDefinition @Result defaultOptions "Api.Result.Result"

instance HasElmDecoder Aeson.Value Result where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @Result defaultOptions Aeson.defaultOptions "Api.Result.decoder"

instance HasElmEncoder Aeson.Value Result where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @Result defaultOptions Aeson.defaultOptions "Api.Result.encoder"

instance (HasElmType a, HasElmType b) => HasElmType (Result a b) where
  elmType =
    Type.apps (elmType @Result) [elmType @a, elmType @b]

instance (HasElmDecoder Aeson.Value a, HasElmDecoder Aeson.Value b) => HasElmDecoder Aeson.Value (Result a b) where
  elmDecoder =
    Expression.apps (elmDecoder @Aeson.Value @Result) [elmDecoder @Aeson.Value @a, elmDecoder @Aeson.Value @b]

instance (HasElmEncoder Aeson.Value a, HasElmDecoder Aeson.Value b) => HasElmEncoder Aeson.Value (Result a b) where
  elmEncoder =
    Expression.apps (elmEncoder @Aeson.Value @Result) [elmEncoder @Aeson.Value @a, elmDecoder @Aeson.Value @b]

main :: IO ()
main = do
  let
    definitions =
      Simplification.simplifyDefinition <$>
        jsonDefinitions @Result

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
