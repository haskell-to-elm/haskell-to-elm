{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
module DerivingVia where

import qualified Data.Aeson as Aeson
import Data.Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.Proxy
import Data.String (fromString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Generics.SOP as SOP
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm

-------------------------------------------------------------------------------
-- A type to derive via, which should typically only be defined once per project.

newtype ElmType (name :: Symbol) a
  = ElmType a

instance
  (Generic a, Aeson.GToJSON Aeson.Zero (Rep a)) =>
  Aeson.ToJSON (ElmType name a)
  where
  toJSON (ElmType a) =
    Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')} a

instance
  (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) =>
  Aeson.FromJSON (ElmType name a)
  where
  parseJSON =
    fmap ElmType . Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}

instance
  (SOP.HasDatatypeInfo a, SOP.All2 HasElmType (SOP.Code a), KnownSymbol name) =>
  HasElmType (ElmType name a)
  where
  elmDefinition =
    Just
      $ deriveElmTypeDefinition @a defaultOptions {fieldLabelModifier = dropWhile (== '_')}
      $ fromString $ symbolVal $ Proxy @name

instance
  (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmDecoder Aeson.Value) (SOP.Code a), HasElmType (ElmType name a), KnownSymbol name) =>
  HasElmDecoder Aeson.Value (ElmType name a)
  where
  elmDecoderDefinition =
    Just
      $ deriveElmJSONDecoder
        @a
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName $ lowerName <> "Decoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = Text.toLower (Text.take 1 name) <> Text.drop 1 name

instance
  (SOP.HasDatatypeInfo a, HasElmType a, SOP.All2 (HasElmEncoder Aeson.Value) (SOP.Code a), HasElmType (ElmType name a), KnownSymbol name) =>
  HasElmEncoder Aeson.Value (ElmType name a)
  where
  elmEncoderDefinition =
    Just
      $ deriveElmJSONEncoder
        @a
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName $ lowerName <> "Encoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = Text.toLower (Text.take 1 name) <> Text.drop 1 name

-------------------------------------------------------------------------------

data User = User
  { _name :: Text
  , _age :: Int
  } deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving (Aeson.ToJSON, Aeson.FromJSON, HasElmType, HasElmDecoder Aeson.Value, HasElmEncoder Aeson.Value) via ElmType "Api.User.User" User

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
