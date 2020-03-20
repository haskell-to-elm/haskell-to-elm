{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module User where

import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Text (Text)
import qualified Generics.SOP as SOP
import GHC.Generics

import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm

data User = User
  { name :: Text
  , age :: Int
  } deriving (Generic, Aeson.ToJSON, SOP.Generic, SOP.HasDatatypeInfo)

instance HasElmType User where
  elmDefinition =
    Just $ deriveElmTypeDefinition @User defaultOptions "Api.User.User"

instance HasElmDecoder Aeson.Value User where
  elmDecoderDefinition =
    Just $ deriveElmJSONDecoder @User defaultOptions Aeson.defaultOptions "Api.User.decoder"

instance HasElmEncoder Aeson.Value User where
  elmEncoderDefinition =
    Just $ deriveElmJSONEncoder @User defaultOptions Aeson.defaultOptions "Api.User.encoder"

main :: IO ()
main = do
  let
    definitions =
      Simplification.simplifyDefinition <$>
        jsonDefinitions @User

    modules =
      Pretty.modules definitions

  forM_ (HashMap.toList modules) $ \(_moduleName, contents) ->
    print contents
