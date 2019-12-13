{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module User where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Generics.SOP as SOP

import qualified Language.Elm.Pretty as Pretty
import Language.Haskell.To.Elm

data User = User
  { name :: Text
  , age :: Int
  } deriving
    ( Generic
    , Aeson.ToJSON
    , SOP.Generic
    , SOP.HasDatatypeInfo
    , HasElmDecoder Aeson.Value
    , HasElmEncoder Aeson.Value
    , HasElmType
    )

instance HasElmDefinition User where
  elmDefinition =
    deriveElmTypeDefinition @User defaultOptions "Api.User.User"

instance HasElmDecoderDefinition Aeson.Value User where
  elmDecoderDefinition =
    deriveElmJSONDecoder @User defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoderDefinition Aeson.Value User where
  elmEncoderDefinition =
    deriveElmJSONEncoder @User defaultOptions Aeson.defaultOptions "Api.User.encoder"

main :: IO ()
main = do
  let
    definitions =
      jsonDefinitions @User

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
