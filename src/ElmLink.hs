{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ElmLink where

import qualified Data.Aeson as Aeson
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics as Generics

{-
  Haskell type -> JSON/String (Aeson, custom)
  JSON/String -> Elm type (elmDecoder)

  Elm type -> JSON/String (elmEncoder)
  JSON/String -> Haskell type (Aeson, custom)
-}

newtype ModuleName = ModuleName Text
  deriving IsString

data QName = QName
  { moduleName :: !ModuleName
  , name :: !Text
  }

instance IsString QName where
  fromString s = 
    let
      (m, n) =
        Text.breakOnEnd "." $ fromString s
    in
    QName (ModuleName m) n


data ElmDef = ElmDef
  { moduleName :: !ModuleName
  , code :: ElmCode
  }

data ElmCode
  = Code !Text
  | Name !QName
  | Empty
  | Codes ElmCode ElmCode
  | Type ElmType

instance Semigroup ElmCode where
  (<>) = Codes

instance Monoid ElmCode where
  mempty = Empty

data ElmType
  = TypeRef !QName
  | TypeApp ElmType ElmType

instance IsString ElmType where
  fromString = TypeRef . fromString

-------------

class HasElmType a where
  elmType :: ElmType
  default elmType :: (Generic a, GHasElmType (Rep a)) => ElmType
  elmType =
    gElmType @(Rep a)

class HasElmDef a where
  elmDef :: ElmDef
  default elmDef :: (Generic a, GHasElmDef (Rep a)) => ElmDef
  elmDef =
    gElmDef @(Rep a)

class HasElmType a => HasElmDecoder a value where
  elmDecoder :: ElmCode
  default elmDecoder :: (Generic a, GHasElmDecoder (Rep a) value) => ElmCode
  elmDecoder =
    gElmDecoder @(Rep a) @value

class HasElmType a => HasElmEncoder a value where
  elmEncoder :: ElmCode
  default elmEncoder :: (Generic a, GHasElmEncoder (Rep a) value) => ElmCode
  elmEncoder =
    gElmEncoder @(Rep a) @value

class GHasElmType (f :: * -> *) where
  gElmType :: ElmType

class GHasElmDef (f :: * -> *) where
  gElmDef :: ElmDef

class GHasElmDef' (f :: * -> *) where
  gElmDef' :: ElmCode

class GHasElmDecoder (f :: * -> *) value where
  gElmDecoder :: ElmCode

class GHasElmEncoder (f :: * -> *) value where
  gElmEncoder :: ElmCode

-------------

instance HasElmType Text where
  elmType =
    "String.String"

instance HasElmEncoder Text Text where
  elmEncoder =
    Name "Basics.identity"

instance HasElmDecoder Text Text where
  elmDecoder =
    Name "Basics.identity"

instance HasElmEncoder Text Aeson.Value where
  elmEncoder =
    Name "Json.Encode.string"

instance HasElmDecoder Text Aeson.Value where
  elmDecoder =
    Name "Json.Decode.string"

instance HasElmType a => HasElmType (Maybe a) where
  elmType =
    TypeApp "Maybe.Maybe" (elmType @a)

instance HasElmEncoder a Aeson.Value => HasElmEncoder (Maybe a) Aeson.Value where
  elmEncoder
    = Name "Maybe.Extra.unwrap" <> Name "Json.Encode.null" <> elmEncoder @a @Aeson.Value

instance HasElmType a => HasElmType [a] where
  elmType =
    TypeApp "List.List" (elmType @a)


-------------

-- Data type
instance (Datatype d, GHasElmDef' a) => GHasElmDef (D1 d a) where
  gElmDef =
    let
      mname = fromString $ Generics.moduleName (undefined :: M1 _ d _ _)

      tname = Code $ fromString $ Generics.datatypeName (undefined :: M1 _ d _ _)
    in
    ElmDef
      { moduleName = mname
      , code = tname <> Code "=" <> gElmDef' @a
      }

-- Single constructor
instance (Constructor c, GHasElmDef' a) => GHasElmDef' (C1 c a) where
  gElmDef' =
    let
      cname = fromString $ Generics.conName (undefined :: M1 _ c _ _)
    in
    Code cname <> gElmDef' @a

-- Multiple constructors
instance (GHasElmDef' f, GHasElmDef' g) => GHasElmDef' (f :+: g) where
  gElmDef' =
    gElmDef' @f <> Code "|" <> gElmDef' @g

-- No constructor fields
instance GHasElmDef' U1 where
  gElmDef' = Empty

-- Multiple constructor fields
instance (GHasElmDef' f, GHasElmDef' g) => GHasElmDef' (f :*: g) where
  gElmDef' =
    gElmDef' @f <> gElmDef' @g

-- Recursion
instance (HasElmType a) => GHasElmDef' (Rec0 a) where
  gElmDef' = Type $ elmType @a

-------------

data Test = A Int Float | B Text
  deriving (Generic)

instance HasElmDef Test where
